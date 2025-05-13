/*
 * Copyright (C) 2020  Data Intuitive
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package io.viash.runners

import io.viash.config.Config
import io.viash.config.BuildInfo

// todo: remove
import io.viash.config.resources.Executable
import io.viash.config.resources.BashScript
import io.viash.config.arguments.{FileArgument, Input, Output}

import io.viash.engines._
import io.viash.engines.requirements.DockerRequirements
import io.viash.runners.executable._

import io.viash.wrapper.BashWrapper
import io.viash.wrapper.BashWrapperMods

import io.viash.helpers.Bash
import io.viash.helpers.data_structures._

import io.viash.schemas._

@description(
  """Run code as an executable.
    |
    |This runner is the default runner. It will generate a bash script that can be run directly.
    |
    |This runner is also used for the @[native](native_engine) engine.
    |
    |This runner is also used for the @[docker](docker_engine) engine.
    |""")
@example(
  """runners:
    |  - type: executable
    |    port: 8080
    |""",
  "yaml")
@subclass("executable")
final case class ExecutableRunner(
  @description("Name of the runner. As with all runners, you can give an runner a different name. By specifying `id: foo`, you can target this executor (only) by specifying `...` in any of the Viash commands.")
  @example("id: foo", "yaml")
  @default("executable")
  id: String = "executable",

  @description("A list of enabled ports. This doesn't change the Dockerfile but gets added as a command-line argument at runtime.")
  @example(
    """port:
      |  - 80
      |  - 8080
      |""",
      "yaml")
  @default("Empty")
  port: OneOrMore[String] = Nil,

  @description("The working directory when starting the engine. This doesn't change the Dockerfile but gets added as a command-line argument at runtime.")
  @example("workdir: /home/user", "yaml")
  workdir: Option[String] = None,

  @description(
    """The Docker setup strategy to use when building a docker engine enrivonment.
      |
      || Strategy | Description |
      ||-----|----------|
      || `alwaysbuild` / `build` / `b` | Always build the image from the dockerfile. This is the default setup strategy.
      || `alwayscachedbuild` / `cachedbuild` / `cb` | Always build the image from the dockerfile, with caching enabled.
      || `ifneedbebuild` |  Build the image if it does not exist locally.
      || `ifneedbecachedbuild` | Build the image with caching enabled if it does not exist locally, with caching enabled.
      || `alwayspull` / `pull` / `p` |  Try to pull the container from [Docker Hub](https://hub.docker.com) or the @[specified docker registry](docker_registry).
      || `alwayspullelsebuild` / `pullelsebuild` |  Try to pull the image from a registry and build it if it doesn't exist.
      || `alwayspullelsecachedbuild` / `pullelsecachedbuild` |  Try to pull the image from a registry and build it with caching if it doesn't exist.
      || `ifneedbepull` |  If the image does not exist locally, pull the image.
      || `ifneedbepullelsebuild` |  If the image does not exist locally, pull the image. If the image does exist, build it.
      || `ifneedbepullelsecachedbuild` | If the image does not exist locally, pull the image. If the image does exist, build it with caching enabled.
      || `push` | Push the container to [Docker Hub](https://hub.docker.com)  or the @[specified docker registry](docker_registry).
      || `pushifnotpresent` | Push the container to [Docker Hub](https://hub.docker.com) or the @[specified docker registry](docker_registry) if the @[tag](docker_tag) does not exist yet.
      || `donothing` / `meh` | Do not build or pull anything.
      |
      |""")
  @example("setup_strategy: alwaysbuild", "yaml")
  @default("ifneedbepullelsecachedbuild")
  docker_setup_strategy: DockerSetupStrategy = IfNeedBePullElseCachedBuild,

  @description("The Apptainer setup strategy to use when building an Apptainer engine environment. Similar to Docker: alwaysbuild, alwayspull, alwayspullelsebuild, ifneedbebuild, ifneedbepull, ifneedbepullelsebuild, donothing.")
  @example("apptainer_setup_strategy: alwaysbuild", "yaml")
  @default("ifneedbepullelsebuild") // A sensible default for Apptainer
  apptainer_setup_strategy: String = "ifneedbepullelsebuild",

  @description("Provide runtime arguments to Docker. See the documentation on [`docker run`](https://docs.docker.com/engine/reference/run/) for more information.")
  @default("Empty")
  docker_run_args: OneOrMore[String] = Nil,

  `type`: String = "executable"
) extends Runner {

  // TODO: Create a proper sealed trait for ApptainerSetupStrategy if more complex logic is needed
  // For now, string matching is used in ViashApptainerFuns.sh

  def generateRunner(config: Config, testing: Boolean): RunnerResources = {
    val engines = config.engines
    

    /*
     * Construct bashwrappermods
     */
    var mods =
      generateEngineVariable(config) ++
      nativeConfigMods(config)

    // Conditionally add engine-specific mods
    mods = mods ++ dockerConfigMods(config, testing)
    mods = mods ++ apptainerConfigMods(config, testing)

    // create new bash script
    val mainScript = Some(BashScript(
      dest = Some(config.name),
      text = Some(BashWrapper.wrapScript(
        executor = "eval $VIASH_CMD",
        mods = mods,
        config = config
      ))
    ))

    // return output
    RunnerResources(
      mainScript = mainScript,
      additionalResources = config.additionalResources
    )
  }

  private def oneOfEngines(engines: List[Engine]): String = {
    engines
      .map(engine => s""" [ "$$VIASH_ENGINE_ID" == "${engine.id}" ] """)
      .mkString(" || ")
  }

  private def noneOfEngines(engines: List[Engine]): String = {
    engines
      .map(engine => s"""" [ $$VIASH_ENGINE_ID" != "${engine.id}" ] """)
      .mkString(" && ")
  }

  private def generateEngineVariable(config: Config): BashWrapperMods = {
    val engines = config.engines

    // TODO: allow setting the default engine
    val preParse = 
      s"""
        |# initialise variables
        |VIASH_MODE='run'
        |VIASH_ENGINE_ID='${engines.head.id}'""".stripMargin

    val parsers =
      s"""
        |        ---engine)
        |            VIASH_ENGINE_ID="$$2"
        |            shift 2
        |            ;;
        |        ---engine=*)
        |            VIASH_ENGINE_ID="$$(ViashRemoveFlags "$$1")"
        |            shift 1
        |            ;;""".stripMargin

    val helpStrings = 
      s"""Viash built in Engines:
         |    ---engine=ENGINE_ID
         |        Specify the engine to use. Options are: ${engines.map(_.id).mkString(", ")}.
         |        Default: ${engines.head.id}""".stripMargin

    val typeSetterStrs = engines.groupBy(_.`type`).map{ case (engineType, engineList) => 
      s""" ${oneOfEngines(engineList)} ; then
        |  VIASH_ENGINE_TYPE='${engineType}'""".stripMargin
    }.toList ++ engines.groupBy(_.`type`).flatMap { // Ensure apptainer is also handled
      case ("apptainer", engineList) => Some(s""" ${oneOfEngines(engineList)} ; then
                                             |  VIASH_ENGINE_TYPE='apptainer'""".stripMargin)
      case _ => None
    }
    val postParse =
      s"""
        |if ${typeSetterStrs.mkString("\nelif ")}
        |else
        |  ViashError "Engine '$$VIASH_ENGINE_ID' is not recognized. Options are: ${engines.map(_.id).mkString(", ")}."
        |  exit 1
        |fi""".stripMargin

    BashWrapperMods(
      preParse = preParse,
      helpStrings = List(("Engine", helpStrings)),
      parsers = parsers,
      postParse = postParse
    )
  }

  private def nativeConfigMods(config: Config): BashWrapperMods = {
    val engines = config.engines.flatMap{
      case e: NativeEngine => Some(e)
      case _ => None
    }

    if (engines.isEmpty) {
      return BashWrapperMods()
    }

    // eval already present, so an executable runs with `eval x` while scripts run with `eval bash x`
    val cmd = config.mainScript match {
      case Some(_: Executable) => ""
      case _ => "bash"
    }

    val preRun =
      s"""
        |if ${oneOfEngines(engines)} ; then
        |  if [ "$$VIASH_MODE" == "run" ]; then
        |    VIASH_CMD="$cmd"
        |  else
        |    ViashError "Engine '$$VIASH_ENGINE_ID' does not support mode '$$VIASH_MODE'."
        |    exit 1
        |  fi
        |fi""".stripMargin
      
    BashWrapperMods(
      preRun = preRun
    )
  }

  /*
   * DOCKER MODS
   */
  private def dockerConfigMods(config: Config, testing: Boolean): BashWrapperMods = {
    val engines = config.engines.flatMap{
      case e: DockerEngine => Some(e)
      case _ => None
    }

    if (engines.isEmpty) {
      return BashWrapperMods()
    }
    
    // generate docker container setup code
    val dmSetup = dockerGenerateSetup(config, config.build_info, testing, engines)

    // generate automount code
    val dmVol = dockerDetectMounts(config)

    // add ---chown flag
    val dmChown = dockerAddChown(config)

    // process cpus and memory_b
    val dmReqs = dockerAddComputationalRequirements()

    // generate docker command
    val dmCmd = dockerGenerateCommand(config)

    // compile bashwrappermods for Docker
    dmSetup ++ dmVol ++ dmChown ++ dmReqs ++ dmCmd
  }

  /*
   * APPTAINER MODS
   */
  private def apptainerConfigMods(config: Config, testing: Boolean): BashWrapperMods = {
    val engines = config.engines.flatMap {
      case e: ApptainerEngine => Some(e)
      case _ => None
    }

    if (engines.isEmpty) {
      return BashWrapperMods()
    }

    val amSetup = apptainerGenerateSetup(config, config.build_info, testing, engines)
    val amVol = apptainerDetectMounts(config) // Apptainer handles mounts differently, but good to have a placeholder
    val amCmd = apptainerGenerateCommand(config)

    amSetup ++ amVol ++ amCmd
  }

  private def apptainerGenerateSetup(
    config: Config,
    info: Option[BuildInfo],
    testing: Boolean,
    engines: List[ApptainerEngine]
  ): BashWrapperMods = {
    val commandsToCheck = config.requirements.commands ::: List("bash") // Basic check
    val commandsToCheckStr = commandsToCheck.mkString("'", "' '", "'")

    val definitionFiles = engines.map { engine =>
      s"""
        |  if [[ "$$engine_id" == "${engine.id}" ]]; then
        |    cat << 'VIASHAPPTAINERDEF'
        |${engine.definitionFile(config, info, testing)}
        |VIASHAPPTAINERDEF
        |  fi""".stripMargin
    }

    // Apptainer build options, e.g., --fakeroot. Could be configurable in ApptainerEngine.
    // For now, hardcode --fakeroot as it's commonly needed for unprivileged builds.
    val apptainerBuildOpts = engines.map { engine =>
      s"""
        |  if [[ "$$engine_id" == "${engine.id}" ]]; then
        |    echo "--fakeroot" # Default to --fakeroot, can be made configurable
        |  fi""".stripMargin
    }

    val preParse =
      s"""${Bash.ViashApptainerFuns}
        |
        |# ViashApptainerDefinitionFile: print the Apptainer definition file to stdout
        |# $$1    : engine identifier
        |# return : Apptainer definition file required to run this component
        |function ViashApptainerDefinitionFile {
        |  local engine_id="$$1"
        |${definitionFiles.mkString}
        |}
        |
        |# ViashApptainerBuildOpts: return the arguments to pass to apptainer build
        |# $$1    : engine identifier
        |# return : arguments to pass to apptainer build
        |function ViashApptainerBuildOpts {
        |  local engine_id="$$1"
        |${apptainerBuildOpts.mkString}
        |}""".stripMargin

    val parsers =
      s"""
        |        ---setup) # This will be caught by Docker's setup first if Docker is an option.
        |            if [[ "$$VIASH_ENGINE_TYPE" == "apptainer" ]]; then
        |              VIASH_MODE='setup'
        |              VIASH_SETUP_STRATEGY="$$2"
        |              shift 2
        |            fi
        |            ;;
        |        ---setup=*)
        |            if [[ "$$VIASH_ENGINE_TYPE" == "apptainer" ]]; then
        |              VIASH_MODE='setup'
        |              VIASH_SETUP_STRATEGY="$$(ViashRemoveFlags "$$1")"
        |              shift 1
        |            fi
        |            ;;
        |        ---definitionfile)
        |            if [[ "$$VIASH_ENGINE_TYPE" == "apptainer" ]]; then
        |              VIASH_MODE='definitionfile'
        |              shift 1
        |            fi
        |            ;;
        |        ---apptainer_image_id)
        |            if [[ "$$VIASH_ENGINE_TYPE" == "apptainer" ]]; then
        |              VIASH_MODE='apptainer_image_id'
        |              shift 1
        |            fi
        |            ;;
        |        ---debug) # Also potentially caught by Docker
        |            if [[ "$$VIASH_ENGINE_TYPE" == "apptainer" ]]; then
        |              VIASH_MODE='debug'
        |              shift 1
        |            fi
        |            ;;""".stripMargin

    val helpStrings =
      s"""Viash built in Apptainer:
         |    ---setup=STRATEGY
         |        Setup the Apptainer image. Options: alwaysbuild, alwayspull, alwayspullelsebuild, ifneedbebuild, ifneedbepull, ifneedbepullelsebuild, donothing.
         |        Default: ${apptainer_setup_strategy}
         |    ---definitionfile
         |        Print the Apptainer definition file to stdout.
         |    ---apptainer_image_id
         |        Print the Apptainer image SIF path to stdout.
         |    ---debug
         |        Enter the Apptainer container for debugging purposes (shell).""".stripMargin

    val setApptainerImageId = engines.map { engine =>
      s"""[[ "$$VIASH_ENGINE_ID" == '${engine.id}' ]]; then
        |    VIASH_APPTAINER_IMAGE_ID='${engine.getTargetIdentifier(config)}'
        |    VIASH_APPTAINER_SOURCE_IMAGE='${engine.image}'""".stripMargin
    }.mkString("if ", "\n  elif ", "\n  fi")

    val postParse =
      s"""
        |if [[ "$$VIASH_ENGINE_TYPE" == "apptainer" ]]; then
        |  ViashApptainerInstallationCheck
        |
        |  $setApptainerImageId
        |
        |  if [ "$$VIASH_MODE" == "definitionfile" ]; then
        |    ViashApptainerDefinitionFile "$$VIASH_ENGINE_ID"
        |    exit 0
        |  elif [ "$$VIASH_MODE" == "apptainer_image_id" ]; then
        |    echo "$$VIASH_APPTAINER_IMAGE_ID"
        |    exit 0
        |  elif [[ "$$VIASH_MODE" == "debug" ]]; then
        |    # Ensure image exists before debugging
        |    ViashApptainerSetup "$$VIASH_APPTAINER_IMAGE_ID" "$$VIASH_APPTAINER_SOURCE_IMAGE" "${apptainer_setup_strategy}" "$$(ViashApptainerBuildOpts "$$VIASH_ENGINE_ID")" "ViashApptainerDefinitionFile \"$$VIASH_ENGINE_ID\""
        |    VIASH_CMD="apptainer shell $$VIASH_APPTAINER_IMAGE_ID"
        |    ViashNotice "+ $$VIASH_CMD"
        |    eval $$VIASH_CMD
        |    exit
        |  elif [ "$$VIASH_MODE" == "setup" ]; then
        |    ViashApptainerSetup "$$VIASH_APPTAINER_IMAGE_ID" "$$VIASH_APPTAINER_SOURCE_IMAGE" "$$VIASH_SETUP_STRATEGY" "$$(ViashApptainerBuildOpts "$$VIASH_ENGINE_ID")" "ViashApptainerDefinitionFile \"$$VIASH_ENGINE_ID\""
        |    ViashApptainerCheckCommands "$$VIASH_APPTAINER_IMAGE_ID" $commandsToCheckStr
        |    exit 0
        |  fi
        |
        |  ViashApptainerSetup "$$VIASH_APPTAINER_IMAGE_ID" "$$VIASH_APPTAINER_SOURCE_IMAGE" "${apptainer_setup_strategy}" "$$(ViashApptainerBuildOpts "$$VIASH_ENGINE_ID")" "ViashApptainerDefinitionFile \"$$VIASH_ENGINE_ID\""
        |  ViashApptainerCheckCommands "$$VIASH_APPTAINER_IMAGE_ID" $commandsToCheckStr
        |fi""".stripMargin

    BashWrapperMods(
      preParse = preParse,
      helpStrings = List(("Apptainer", helpStrings)),
      parsers = parsers,
      postParse = postParse
    )
  }

  private def dockerGenerateSetup(
    config: Config,
    info: Option[BuildInfo],
    testing: Boolean,
    engines: List[DockerEngine]
  ): BashWrapperMods = {
    
    // get list of all the commands that should be available in the container
    val commandsToCheck = config.requirements.commands ::: List("bash")
    val commandsToCheckStr = commandsToCheck.mkString("'", "' '", "'")

    val dockerFiles = engines.map { engine =>
      s"""
        |  if [[ "$$engine_id" == "${engine.id}" ]]; then
        |    cat << 'VIASHDOCKER'
        |${engine.dockerFile(config, info, testing)}
        |VIASHDOCKER
        |  fi""".stripMargin
    }

    val dockerBuildArgs = engines.map { engine =>
      val setups = engine.setup ::: { if (testing) engine.test_setup else Nil }
      val dockerRequirements = 
        setups.flatMap{
          case d: DockerRequirements => d.build_args
          case _ => Nil
        }
      val buildArgs = dockerRequirements.map("--build-arg '" + _ + "'").mkString(" ")

      s"""
        |  if [[ "$$engine_id" == "${engine.id}" ]]; then
        |    echo "${buildArgs}"
        |  fi""".stripMargin
    }
    
    val preParse =
      s"""${Bash.ViashDockerFuns}
        |
        |# ViashDockerFile: print the dockerfile to stdout
        |# $$1    : engine identifier
        |# return : dockerfile required to run this component
        |# examples:
        |#   ViashDockerFile
        |function ViashDockerfile {
        |  local engine_id="$$1"
        |${dockerFiles.mkString}
        |}
        |
        |# ViashDockerBuildArgs: return the arguments to pass to docker build
        |# $$1    : engine identifier
        |# return : arguments to pass to docker build
        |function ViashDockerBuildArgs {
        |  local engine_id="$$1"
        |${dockerBuildArgs.mkString}
        |}""".stripMargin

    val parsers =
      s"""
        |        ---setup)
        |            VIASH_MODE='setup'
        |            VIASH_SETUP_STRATEGY="$$2"
        |            shift 2
        |            ;;
        |        ---setup=*)
        |            VIASH_MODE='setup'
        |            VIASH_SETUP_STRATEGY="$$(ViashRemoveFlags "$$1")"
        |            shift 1
        |            ;;
        |        ---dockerfile)
        |            VIASH_MODE='dockerfile'
        |            shift 1
        |            ;;
        |        ---docker_run_args)
        |            VIASH_DOCKER_RUN_ARGS+=("$$2")
        |            shift 2
        |            ;;
        |        ---docker_run_args=*)
        |            VIASH_DOCKER_RUN_ARGS+=("$$(ViashRemoveFlags "$$1")")
        |            shift 1
        |            ;;
        |        ---docker_image_id)
        |            VIASH_MODE='docker_image_id'
        |            shift 1
        |            ;;
        |        ---debug)
        |            VIASH_MODE='debug'
        |            shift 1
        |            ;;""".stripMargin

    val helpStrings = 
      s"""Viash built in Docker:
         |    ---setup=STRATEGY
         |        Setup the docker container. Options are: alwaysbuild, alwayscachedbuild, ifneedbebuild, ifneedbecachedbuild, alwayspull, alwayspullelsebuild, alwayspullelsecachedbuild, ifneedbepull, ifneedbepullelsebuild, ifneedbepullelsecachedbuild, push, pushifnotpresent, donothing.
         |        Default: ifneedbepullelsecachedbuild
         |    ---dockerfile
         |        Print the dockerfile to stdout.
         |    ---docker_run_args=ARG
         |        Provide runtime arguments to Docker. See the documentation on `docker run` for more information.
         |    ---docker_image_id
         |        Print the docker image id to stdout.
         |    ---debug
         |        Enter the docker container for debugging purposes.""".stripMargin

    val setDockerImageId = engines.map { engine => 
      s"""[[ "$$VIASH_ENGINE_ID" == '${engine.id}' ]]; then
        |    VIASH_DOCKER_IMAGE_ID='${engine.getTargetIdentifier(config).toString()}'""".stripMargin  
    }.mkString("if ", "\n  elif ", "\n  fi")

    val postParse =
      s"""
        |if [[ "$$VIASH_ENGINE_TYPE" == "docker" ]]; then
        |  # check if docker is installed properly
        |  ViashDockerInstallationCheck
        |
        |  # determine docker image id
        |  $setDockerImageId
        |
        |  # print dockerfile
        |  if [ "$$VIASH_MODE" == "dockerfile" ]; then
        |    ViashDockerfile "$$VIASH_ENGINE_ID"
        |    exit 0
        |
        |  elif [ "$$VIASH_MODE" == "docker_image_id" ]; then
        |    echo "$$VIASH_DOCKER_IMAGE_ID"
        |    exit 0
        |  
        |  # enter docker container
        |  elif [[ "$$VIASH_MODE" == "debug" ]]; then
        |    VIASH_CMD="docker run --entrypoint=bash $${VIASH_DOCKER_RUN_ARGS[@]} -v '$$(pwd)':/pwd --workdir /pwd -t $$VIASH_DOCKER_IMAGE_ID"
        |    ViashNotice "+ $$VIASH_CMD"
        |    eval $$VIASH_CMD
        |    exit 
        |
        |  # build docker image
        |  elif [ "$$VIASH_MODE" == "setup" ]; then
        |    ViashDockerSetup "$$VIASH_DOCKER_IMAGE_ID" "$$VIASH_SETUP_STRATEGY"
        |    ViashDockerCheckCommands "$$VIASH_DOCKER_IMAGE_ID" $commandsToCheckStr
        |    exit 0
        |  fi
        |
        |  # check if docker image exists
        |  ViashDockerSetup "$$VIASH_DOCKER_IMAGE_ID" ${docker_setup_strategy.id}
        |  ViashDockerCheckCommands "$$VIASH_DOCKER_IMAGE_ID" $commandsToCheckStr
        |fi""".stripMargin

          
    BashWrapperMods(
      preParse = preParse,
      helpStrings = List(("Docker", helpStrings)),
      parsers = parsers,
      postParse = postParse
    )
  }

  

  private def dockerDetectMounts(config: Config): BashWrapperMods = {
    val args = config.getArgumentLikes(includeMeta = true)
    
    val detectMounts = args.flatMap {
      case arg: FileArgument if arg.multiple =>
        // resolve arguments with multiplicity different from singular args
        val viash_temp = "VIASH_TEST_" + arg.plainName.toUpperCase()
        val chownIfOutput = if (arg.direction == Output) "\n    VIASH_CHOWN_VARS+=( \"$var\" )" else ""
        Some(
          s"""
            |if [ ! -z "$$${arg.VIASH_PAR}" ]; then
            |  $viash_temp=()
            |  IFS='${Bash.escapeString(arg.multiple_sep, quote = true)}'
            |  for var in $$${arg.VIASH_PAR}; do
            |    unset IFS
            |    VIASH_DIRECTORY_MOUNTS+=( "$$(ViashDockerAutodetectMountArg "$$var")" )
            |    var=$$(ViashDockerAutodetectMount "$$var")
            |    $viash_temp+=( "$$var" )$chownIfOutput
            |  done
            |  ${arg.VIASH_PAR}=$$(IFS='${Bash.escapeString(arg.multiple_sep, quote = true)}' ; echo "$${$viash_temp[*]}")
            |fi""".stripMargin
        )
      case arg: FileArgument =>
        val chownIfOutput = if (arg.direction == Output) "\n  VIASH_CHOWN_VARS+=( \"$" + arg.VIASH_PAR + "\" )" else ""
        Some(
          s"""
            |if [ ! -z "$$${arg.VIASH_PAR}" ]; then
            |  VIASH_DIRECTORY_MOUNTS+=( "$$(ViashDockerAutodetectMountArg "$$${arg.VIASH_PAR}")" )
            |  ${arg.VIASH_PAR}=$$(ViashDockerAutodetectMount "$$${arg.VIASH_PAR}")$chownIfOutput
            |fi""".stripMargin
        )
      case _ => None
    }

    // if there are no mounts, return empty mods
    if (detectMounts.isEmpty) {
      return BashWrapperMods()
    }

    val preParse =
      s"""
         |${Bash.ViashAbsolutePath}
         |${Bash.ViashDockerAutodetectMount}
         |# initialise variables
         |VIASH_DIRECTORY_MOUNTS=()
         |
         |# configure default docker automount prefix if it is unset
         |if [ -z "$${VIASH_DOCKER_AUTOMOUNT_PREFIX+x}" ]; then
         |  VIASH_DOCKER_AUTOMOUNT_PREFIX="/viash_automount"
         |fi""".stripMargin
    
    val preRun =
      f"""
        |if [[ "$$VIASH_ENGINE_TYPE" == "docker" ]]; then
        |  # detect volumes from file arguments
        |  VIASH_CHOWN_VARS=()${detectMounts.mkString("")}
        |  
        |  # get unique mounts
        |  VIASH_UNIQUE_MOUNTS=($$(for val in "$${VIASH_DIRECTORY_MOUNTS[@]}"; do echo "$$val"; done | sort -u))
        |fi
        |""".stripMargin
    
  
    val stripAutomounts = args.flatMap {
      case arg: FileArgument if arg.multiple && arg.direction == Input =>
        // resolve arguments with multiplicity different from singular args
        val viash_temp = "VIASH_TEST_" + arg.plainName.toUpperCase()
        Some(
          s"""
            |  if [ ! -z "$$${arg.VIASH_PAR}" ]; then
            |    unset $viash_temp
            |    IFS='${Bash.escapeString(arg.multiple_sep, quote = true)}'
            |    for var in $$${arg.VIASH_PAR}; do
            |      unset IFS
            |      ${BashWrapper.store("ViashDockerStripAutomount", viash_temp, "\"$(ViashDockerStripAutomount \"$var\")\"", Some(arg.multiple_sep)).mkString("\n    ")}
            |    done
            |    ${arg.VIASH_PAR}="$$$viash_temp"
            |  fi""".stripMargin
        )
      case arg: FileArgument =>
        Some(
          s"""
            |  if [ ! -z "$$${arg.VIASH_PAR}" ]; then
            |    ${arg.VIASH_PAR}=$$(ViashDockerStripAutomount "$$${arg.VIASH_PAR}")
            |  fi""".stripMargin
        )
      case _ => None
    }
    
    val postRun =
      s"""
        |if [[ "$$VIASH_ENGINE_TYPE" == "docker" ]]; then
        |  # strip viash automount from file paths
        |  ${stripAutomounts.mkString("")}
        |fi""".stripMargin

    BashWrapperMods(
      preParse = preParse,
      preRun = preRun,
      postRun = postRun
    )
  }

  private def dockerAddChown(config: Config): BashWrapperMods = {
    // TODO: how are mounts added to this section?
    val preRun =
      s"""
        |if [[ "$$VIASH_ENGINE_TYPE" == "docker" ]]; then
        |  # change file ownership
        |  function ViashPerformChown {
        |    if (( $${#VIASH_CHOWN_VARS[@]} )); then
        |      set +e
        |      VIASH_CMD="docker run --entrypoint=bash --rm $${VIASH_UNIQUE_MOUNTS[@]} $$VIASH_DOCKER_IMAGE_ID -c 'chown $$(id -u):$$(id -g) --silent --recursive $${VIASH_CHOWN_VARS[@]}'"
        |      ViashDebug "+ $$VIASH_CMD"
        |      eval $$VIASH_CMD
        |      set -e
        |    fi
        |  }
        |  trap ViashPerformChown EXIT
        |fi""".stripMargin

    BashWrapperMods(
      preRun = preRun
    )
  }

  private def dockerAddComputationalRequirements(): BashWrapperMods = {
    // add requirements to parameters
    val preRun = 
      s"""
        |if [[ "$$VIASH_ENGINE_TYPE" == "docker" ]]; then
        |  # helper function for filling in extra docker args
        |  if [ ! -z "$$VIASH_META_MEMORY_B" ]; then
        |    VIASH_DOCKER_RUN_ARGS+=("--memory=$${VIASH_META_MEMORY_B}")
        |  fi
        |  if [ ! -z "$$VIASH_META_CPUS" ]; then
        |    VIASH_DOCKER_RUN_ARGS+=("--cpus=$${VIASH_META_CPUS}")
        |  fi
        |fi""".stripMargin

    // return output
    BashWrapperMods(
      preRun = preRun
    )
  }

  private def dockerGenerateCommand(config: Config): BashWrapperMods = {

    // collect runtime docker arguments
    val entrypointStr = config.mainScript match {
      case Some(_: Executable) => " --entrypoint=''"
      case _ => " --entrypoint=bash"
    }

    val workdirStr = workdir.map(" --workdir '" + _ + "'").getOrElse("")

    val dockerArgs =
      "-i --rm" +
        port.map(" -p " + _).mkString +
        docker_run_args.map(" " + _).mkString
    
    val preParse = 
      s"""
        |# initialise docker variables
        |VIASH_DOCKER_RUN_ARGS=($dockerArgs)""".stripMargin

    val preRun =
      s"""
        |if [[ "$$VIASH_ENGINE_TYPE" == "docker" ]]; then
        |  VIASH_CMD="docker run$entrypointStr$workdirStr $${VIASH_DOCKER_RUN_ARGS[@]} $${VIASH_UNIQUE_MOUNTS[@]} $$VIASH_DOCKER_IMAGE_ID"
        |fi""".stripMargin
      
    BashWrapperMods(
      preParse = preParse,
      preRun = preRun
    )
  }

  private def apptainerDetectMounts(config: Config): BashWrapperMods = {
    val args = config.getArgumentLikes(includeMeta = true)

    // Apptainer automatically mounts $HOME, /tmp, cwd etc.
    // Explicit binds are via -B /src:/dest or -B /path (if src=dest)
    // This logic will be simpler than Docker's custom automount prefix.
    // We just need to ensure paths are absolute for -B.
    val detectMounts = args.flatMap {
      case arg: FileArgument =>
        // For Apptainer, we mainly need to ensure the paths exist and are accessible.
        // Absolute paths are preferred for explicit binds if needed.
        // We'll collect paths that might need explicit binding if they are outside standard bind locations.
        // For now, this is a simplified version. A more robust solution would check if paths
        // are already covered by Apptainer's default binds.
        Some(
          s"""
            |if [ ! -z "$$${arg.VIASH_PAR}" ]; then
            |  # Convert to absolute path for potential binding
            |  abs_path=$$(ViashAbsolutePath "$$${arg.VIASH_PAR}")
            |  # Add to a list of paths to consider for -B, if not standard (e.g. not under /home, /tmp, cwd)
            |  # This is a placeholder for more complex logic. For now, just pass it.
            |  # Apptainer is quite good at finding paths.
            |  # If explicit binding is needed, it would be:
            |  # VIASH_APPTAINER_BINDS+=( "-B $$abs_path" )
            |fi""".stripMargin
        )
      case _ => None
    }

    if (detectMounts.isEmpty) {
      return BashWrapperMods()
    }

    val preParse =
      s"""
         |${Bash.ViashAbsolutePath} # Already included by Docker, but good for standalone
         |VIASH_APPTAINER_BINDS=()""".stripMargin

    val preRun =
      s"""
        |if [[ "$$VIASH_ENGINE_TYPE" == "apptainer" ]]; then${detectMounts.mkString("")}
        |  # VIASH_APPTAINER_BINDS will contain -B options if any were explicitly added.
        |fi
        |""".stripMargin

    BashWrapperMods(
      preParse = preParse,
      preRun = preRun
    )
  }

  private def apptainerGenerateCommand(config: Config): BashWrapperMods = {
    val preRun =
      s"""
        |if [[ "$$VIASH_ENGINE_TYPE" == "apptainer" ]]; then
        |  # VIASH_APPTAINER_BINDS array can be used here if explicit binds are constructed
        |  VIASH_CMD="apptainer exec $${VIASH_APPTAINER_BINDS[@]} $$VIASH_APPTAINER_IMAGE_ID"
        |fi""".stripMargin
    BashWrapperMods(preRun = preRun)
  }
}
