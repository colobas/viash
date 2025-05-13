package io.viash.runners

import org.scalatest.funsuite.AnyFunSuite
import io.viash.config._
import io.viash.config.resources._
import io.viash.engines._
import io.viash.engines.requirements._
import io.viash.helpers.Logger
import io.viash.helpers.data_structures.listToOneOrMore

class ApptainerRunnerTest extends AnyFunSuite {
  Logger.UseColorOverride.value = Some(false)

  test("ExecutableRunner with ApptainerEngine generates correct bash wrapper") {
    val config = Config(
      name = "test_component",
      resources = List(
        BashScript(
          text = Some("echo 'Hello World'"),
          dest = Some("script.sh")
        )
      ),
      engines = List(
        ApptainerEngine(
          image = "docker://ubuntu:20.04"
        )
      )
    )

    val runner = ExecutableRunner()
    val resources = runner.generateRunner(config, false)

    // Check that the main script is generated
    assert(resources.mainScript.isDefined)

    // Check that the script contains Apptainer-specific code
    val scriptText = resources.mainScript.get.text.get

    // Check for Apptainer functions
    assert(scriptText.contains("function ViashApptainerInstallationCheck"))
    assert(scriptText.contains("function ViashApptainerRemoteImageCheck"))
    assert(scriptText.contains("function ViashApptainerLocalImageCheck"))
    assert(scriptText.contains("function ViashApptainerDefinitionFile"))
    assert(scriptText.contains("function ViashApptainerBuildOpts"))
    assert(scriptText.contains("VIASH_APPTAINER_IMAGE_ID"))
    assert(scriptText.contains("ViashApptainerInstallationCheck"))
    assert(scriptText.contains("ViashApptainerSetup"))

    // Check for Apptainer command construction
    assert(scriptText.contains("VIASH_CMD=\"apptainer exec"))

    // Check for setup strategy
    assert(scriptText.contains("ifneedbepullelsebuild"))

    // Check for help strings
    assert(scriptText.contains("---setup=STRATEGY"))
    assert(scriptText.contains("---definitionfile"))
    assert(scriptText.contains("---apptainer_image_id"))
  }

  test("ExecutableRunner with ApptainerEngine generates definition file") {
    val config = Config(
      name = "test_component",
      resources = List(
        BashScript(
          text = Some("echo 'Hello World'"),
          dest = Some("script.sh")
        )
      ),
      engines = List(
        ApptainerEngine(
          id = "apptainer",
          image = "docker://ubuntu:20.04",
          setup = List(
            AptRequirements(
              packages = listToOneOrMore(List("curl"))
            )
          )
        )
      )
    )

    val engine = config.engines.head.asInstanceOf[ApptainerEngine]
    val defFile = engine.definitionFile(config, None, false)

    // Check basic structure
    assert(defFile.contains("Bootstrap: docker"))
    assert(defFile.contains("From: ubuntu:20.04"))

    // Check for requirements
    assert(defFile.contains("apt-get install -y curl"))

    // Check for labels
    assert(defFile.contains("%labels"))
    assert(defFile.contains("DESCRIPTION"))

    // Check for runscript
    assert(defFile.contains("%runscript"))
    assert(defFile.contains("exec /opt/viash_script/test_component"))
  }

  test("NextflowRunner with ApptainerEngine generates correct Nextflow config") {
    val config = Config(
      name = "test_component",
      resources = List(
        BashScript(
          text = Some("echo 'Hello World'"),
          dest = Some("script.sh")
        )
      ),
      engines = List(
        ApptainerEngine(
          id = "apptainer",
          image = "docker://ubuntu:20.04"
        )
      )
    )

    val runner = NextflowRunner(
      container = "apptainer"
    )

    val containerDirective = runner.containerDirective(config)
    assert(containerDirective.isDefined)
    assert(containerDirective.get.containerType === "apptainer")
    assert(containerDirective.get.name === "test_component.sif")

    val nfConfig = runner.renderNextflowConfig(config, containerDirective)

    // Check for Apptainer profile
    assert(nfConfig.contains("apptainer {"))
    assert(nfConfig.contains("apptainer.enabled = true"))
  }
}
