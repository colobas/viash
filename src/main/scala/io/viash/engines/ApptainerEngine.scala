/*
 * Copyright (C) 2020-2023  Data Intuitive
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

package io.viash.engines

import java.text.SimpleDateFormat
import java.util.Date
import io.viash.config.{Config, BuildInfo, Author}
import io.viash.engines.requirements.Requirements
import io.viash.helpers.Escaper
import io.viash.schemas._

@description(
  """Run a Viash component on an Apptainer (formerly Singularity) backend engine.
    |By specifying which dependencies your component needs, users will be able to build an Apptainer image from scratch using the setup flag.
    |""")
@example(
  """engines:
    |  - type: apptainer
    |    image: "docker://ubuntu:20.04" # Can be docker://, library://, shub://, oras:// or a local SIF path
    |    setup:
    |      - type: apt
    |        packages: [ curl ]
    |""",
  "yaml")
@subclass("apptainer")
final case class ApptainerEngine(
  @description("Name of the engine. As with all engines, you can give an engine a different name. By specifying `id: foo`, you can target this engine (only) by specifying `...` in any of the Viash commands.")
  @example("id: foo", "yaml")
  @default("apptainer")
  id: String = "apptainer",

  @description("The base image to start from. This can be a Docker Hub image (e.g., `docker://ubuntu:20.04`), a Sylabs Cloud library image (e.g., `library://alpine:latest`), Singularity Hub (e.g. `shub://`), OCI registry (e.g. `oras://`) or a path to a local SIF file.")
  @example("image: \"docker://ubuntu:20.04\"", "yaml")
  image: String,

  @description("If anything is specified in the setup section, running `---setup` will result in an image with the name of `<target_image>`. If nothing is specified in the `setup` section, the `image` will be used directly (if it's a local SIF) or pulled. Default: `<component_name>.sif`.")
  @example("target_image: myfoo.sif", "yaml")
  target_image: Option[String] = None,

  @description(
    """A list of requirements for installing packages. These commands will be executed in the `%post` section of the Apptainer definition file.
      |Supported types are similar to Docker: apt, apk, yum, python, r, etc.
      |The order in which these dependencies are specified determines the order in which they will be installed.
      |""")
  @default("Empty")
  setup: List[Requirements] = Nil,

  @description("Additional requirements specific for running unit tests. These are added to the `%post` section if building for tests.")
  @since("Viash 0.5.13") // Assuming similar versioning logic as DockerEngine
  @default("Empty")
  test_setup: List[Requirements] = Nil,

  `type`: String = "apptainer"
) extends Engine {
  val hasSetup = true

  /**
   * Generate an Apptainer definition file for the container.
   *
   * @param config The config.
   * @param info The config info (available).
   _ @param testing Whether or not this container is used as part of a `viash test`.
   * @return The Apptainer definition file as a string.
   */
  def definitionFile(
    config: Config,
    info: Option[BuildInfo],
    testing: Boolean
  ): String = {
    // Determine Bootstrap agent and From value
    val (bootstrapAgent, fromLocation) = image match {
      case img if img.startsWith("docker://") || img.startsWith("library://") || img.startsWith("shub://") || img.startsWith("oras://") =>
        val parts = img.split("://", 2)
        (parts(0), parts(1))
      case localPath if localPath.endsWith(".sif") => // Assuming local SIF file
        ("localimage", localPath)
      case _ => // Default to docker if no prefix
        ("docker", image)
    }

    // Construct labels from metadata
    val authors = config.authors match {
      case Nil => None
      case aut: List[Author] => Some(aut.map(_.name).mkString(", "))
    }
    val apptainerLabels = List(
      authors.map(aut => s"""AUTHOR "${Escaper(aut, quote = true)}""""),
      Some(s"""DESCRIPTION "Companion container for running component ${config.namespace.map(_ + " ").getOrElse("")}${config.name}""""),
      Some(s"""CREATED_AT "${new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX").format(new Date())}""""),
      config.version.map(v => s"""VERSION "$v""""),
      info.flatMap(_.git_commit).map(rev => s"""GIT_COMMIT "$rev""""),
      target_image_source(config, info).map(src => s"""SOURCE_REPO "${Escaper(src, quote = true)}"""")
    ).flatten.mkString("\n    ")

    val allSetup = if (testing) setup ::: test_setup else setup
    val postCommands = allSetup.flatMap(_.installCommands).map("    " + _) // Indent for %post section

    // %environment section: Viash might set some default env vars later, or users can add them via requirements.
    // For now, keep it simple. Sourcing /environment is common for Apptainer.
    val environmentSetup = "    . /environment"

    // %runscript section: This defines what `apptainer run my_image.sif` does.
    // For Viash, we typically use `apptainer exec` with the specific script, so %runscript might be minimal
    // or could default to running the main component script.
    // Let's make it execute the component's main script by default.
    // The actual script path inside the container needs to be determined.
    // Assuming the script is copied to /opt/viash_script/${config.name}
    val runscriptContent = s"""    exec /opt/viash_script/${config.name} "$$@""""

    s"""Bootstrap: $bootstrapAgent
      |From: $fromLocation
      |
      |%labels
      |    $apptainerLabels
      |
      |%post
      |    apt-get update || yum check-update || apk update || true # Common update commands
      |${postCommands.mkString("\n")}
      |    # Clean up package manager caches
      |    apt-get clean && rm -rf /var/lib/apt/lists/* || yum clean all || apk cache clean || true
      |
      |%environment
      |$environmentSetup
      |
      |%runscript
      |$runscriptContent
      |""".stripMargin
  }

  private def target_image_source(config: Config, info: Option[BuildInfo]): Option[String] = {
    config.links.repository
      .orElse(info.flatMap(
        _.git_remote.map(
          _.replaceAll(":([^/])", "/$1")
            .replace("ssh//", "")
            .replace("git@", "https://")
        )
      ))
  }

  def getTargetIdentifier(config: Config): String = {
    target_image.getOrElse(s"${config.name}.sif")
  }
}
