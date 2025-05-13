package io.viash.engines

import org.scalatest.funsuite.AnyFunSuite
import io.viash.config._
import io.viash.helpers.Logger
import io.viash.engines.requirements._
import io.viash.helpers.data_structures.listToOneOrMore

class ApptainerEngineTest extends AnyFunSuite {
  Logger.UseColorOverride.value = Some(false)

  test("ApptainerEngine constructor") {
    val engine = ApptainerEngine(
      image = "docker://ubuntu:20.04"
    )

    assert(engine.id === "apptainer")
    assert(engine.image === "docker://ubuntu:20.04")
    assert(engine.target_image === None)
    assert(engine.setup === Nil)
    assert(engine.test_setup === Nil)
    assert(engine.`type` === "apptainer")
    assert(engine.hasSetup === true)
  }

  test("ApptainerEngine with custom id") {
    val engine = ApptainerEngine(
      id = "custom_apptainer",
      image = "docker://ubuntu:20.04"
    )

    assert(engine.id === "custom_apptainer")
  }

  test("ApptainerEngine with target_image") {
    val engine = ApptainerEngine(
      image = "docker://ubuntu:20.04",
      target_image = Some("custom.sif")
    )

    assert(engine.target_image === Some("custom.sif"))
  }

  test("ApptainerEngine with setup requirements") {
    val engine = ApptainerEngine(
      image = "docker://ubuntu:20.04",
      setup = List(
        AptRequirements(
          packages = listToOneOrMore(List("curl", "wget"))
        )
      )
    )

    assert(engine.setup.length === 1)
    assert(engine.setup.head.isInstanceOf[AptRequirements])
    assert(engine.setup.head.asInstanceOf[AptRequirements].packages === listToOneOrMore(List("curl", "wget")))
  }

  test("getTargetIdentifier with default") {
    val engine = ApptainerEngine(
      image = "docker://ubuntu:20.04"
    )

    val config = Config(
      name = "test_component"
    )

    assert(engine.getTargetIdentifier(config) === "test_component.sif")
  }

  test("getTargetIdentifier with custom target_image") {
    val engine = ApptainerEngine(
      image = "docker://ubuntu:20.04",
      target_image = Some("custom.sif")
    )

    val config = Config(
      name = "test_component"
    )

    assert(engine.getTargetIdentifier(config) === "custom.sif")
  }

  test("definitionFile basic structure") {
    val engine = ApptainerEngine(
      image = "docker://ubuntu:20.04"
    )

    val config = Config(
      name = "test_component"
    )

    val defFile = engine.definitionFile(config, None, false)

    assert(defFile.contains("Bootstrap: docker"))
    assert(defFile.contains("From: ubuntu:20.04"))
    assert(defFile.contains("%labels"))
    assert(defFile.contains("%post"))
    assert(defFile.contains("%environment"))
    assert(defFile.contains("%runscript"))
  }

  test("definitionFile with requirements") {
    val engine = ApptainerEngine(
      image = "docker://ubuntu:20.04",
      setup = List(
        AptRequirements(
          packages = listToOneOrMore(List("curl", "wget"))
        )
      )
    )

    val config = Config(
      name = "test_component"
    )

    val defFile = engine.definitionFile(config, None, false)

    assert(defFile.contains("apt-get install -y curl wget"))
  }

  test("definitionFile with test requirements") {
    val engine = ApptainerEngine(
      image = "docker://ubuntu:20.04",
      setup = List(
        AptRequirements(
          packages = listToOneOrMore(List("curl"))
        )
      ),
      test_setup = List(
        AptRequirements(
          packages = listToOneOrMore(List("wget"))
        )
      )
    )

    val config = Config(
      name = "test_component"
    )

    // Without testing flag
    val defFileNoTest = engine.definitionFile(config, None, false)
    assert(defFileNoTest.contains("apt-get install -y curl"))
    assert(!defFileNoTest.contains("apt-get install -y wget"))
    assert(!defFileNoTest.contains("apt-get install -y wget"))

    // With testing flag
    val defFileWithTest = engine.definitionFile(config, None, true)
    assert(defFileWithTest.contains("apt-get install -y curl"))
    assert(defFileWithTest.contains("apt-get install -y wget"))
  }


  test("definitionFile with different image types") {
    // Docker image
    val dockerEngine = ApptainerEngine(
      image = "docker://ubuntu:20.04"
    )
    val dockerDef = dockerEngine.definitionFile(Config("test"), None, false)
    assert(dockerDef.contains("Bootstrap: docker"))
    assert(dockerDef.contains("From: ubuntu:20.04"))

    // Library image
    val libraryEngine = ApptainerEngine(
      image = "library://alpine:latest"
    )
    val libraryDef = libraryEngine.definitionFile(Config("test"), None, false)
    assert(libraryDef.contains("Bootstrap: library"))
    assert(libraryDef.contains("From: alpine:latest"))

    // Local SIF file
    val localEngine = ApptainerEngine(
      image = "myimage.sif"
    )
    val localDef = localEngine.definitionFile(Config("test"), None, false)
    assert(localDef.contains("Bootstrap: localimage"))
    assert(localDef.contains("From: myimage.sif"))
  }
}
