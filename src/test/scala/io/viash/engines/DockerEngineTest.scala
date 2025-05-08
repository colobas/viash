package io.viash.engines

import io.viash.config.{Config, BuildInfo}
import io.viash.exceptions.ConfigException
import io.viash.helpers.Escaper
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DockerEngineTest extends AnyFunSuite with Matchers {

  val defaultConfig: Config = Config(
    name = "my-component",
    namespace = Some("testspace"),
    version = Some(Config.Version("1.2.3")),
    description = Some("A test component")
  )
  val defaultDockerFilePath = "/tmp/Dockerfile.test"
  val defaultContextPath = "."

  test("getBuildCommand should generate correct command for default Docker builder") {
    val engine = DockerEngine(
      image = "base/image:latest" // Base image for Dockerfile generation, not directly part of build command logic here
    )
    val expectedImage = engine.getTargetIdentifier(defaultConfig).toString
    val command = engine.getBuildCommand(defaultConfig, None, defaultDockerFilePath, defaultContextPath)

    command shouldBe s"docker build -f ${Escaper.shell(defaultDockerFilePath)} -t ${Escaper.shell(expectedImage)} ${Escaper.shell(defaultContextPath)}"
  }

  test("getBuildCommand should generate correct command for Buildah builder") {
    val engine = DockerEngine(
      image = "base/image:latest",
      builder = "buildah"
    )
    val expectedImage = engine.getTargetIdentifier(defaultConfig).toString
    val command = engine.getBuildCommand(defaultConfig, None, defaultDockerFilePath, defaultContextPath)

    command shouldBe s"apptainer run docker://quay.io/buildah/stable:latest buildah bud --format docker -f ${Escaper.shell(defaultDockerFilePath)} -t ${Escaper.shell(expectedImage)} ${Escaper.shell(defaultContextPath)}"
  }

  test("getBuildCommand should use builder_executable_override for Docker") {
    val customDockerPath = "/usr/local/bin/docker-custom"
    val engine = DockerEngine(
      image = "base/image:latest",
      builder = "docker",
      builder_executable_override = Some(customDockerPath)
    )
    val expectedImage = engine.getTargetIdentifier(defaultConfig).toString
    val command = engine.getBuildCommand(defaultConfig, None, defaultDockerFilePath, defaultContextPath)

    command shouldBe s"$customDockerPath build -f ${Escaper.shell(defaultDockerFilePath)} -t ${Escaper.shell(expectedImage)} ${Escaper.shell(defaultContextPath)}"
  }

  test("getBuildCommand should use builder_executable_override for Buildah (e.g. podman)") {
    val customBuildahCommand = "podman build --format docker" // Note: podman uses 'build', not 'bud'
    val engine = DockerEngine(
      image = "base/image:latest",
      builder = "buildah", // Still buildah type, but command is overridden
      builder_executable_override = Some("podman") // Simulating user wants podman for buildah type tasks
    )
    // For this specific override, we expect the command structure to adapt.
    // The getBuildCommand logic for buildah appends "bud --format docker".
    // If the override is just "podman", it becomes "podman bud --format docker".
    // This test verifies the override is prepended correctly.
    val expectedImage = engine.getTargetIdentifier(defaultConfig).toString
    val command = engine.getBuildCommand(defaultConfig, None, defaultDockerFilePath, defaultContextPath)

    command shouldBe s"podman bud --format docker -f ${Escaper.shell(defaultDockerFilePath)} -t ${Escaper.shell(expectedImage)} ${Escaper.shell(defaultContextPath)}"
  }

   test("getBuildCommand should use builder_executable_override for Buildah with full command") {
    val customBuildahCommand = "apptainer run docker://my/custom/buildah:latest buildah"
    val engine = DockerEngine(
      image = "base/image:latest",
      builder = "buildah",
      builder_executable_override = Some(customBuildahCommand)
    )
    val expectedImage = engine.getTargetIdentifier(defaultConfig).toString
    val command = engine.getBuildCommand(defaultConfig, None, defaultDockerFilePath, defaultContextPath)

    command shouldBe s"$customBuildahCommand bud --format docker -f ${Escaper.shell(defaultDockerFilePath)} -t ${Escaper.shell(expectedImage)} ${Escaper.shell(defaultContextPath)}"
  }

  test("getBuildCommand should include additional build arguments") {
    val engine = DockerEngine(
      image = "base/image:latest"
    )
    val additionalArgs = List("--build-arg", "FOO=bar", "--no-cache")
    val expectedImage = engine.getTargetIdentifier(defaultConfig).toString
    val command = engine.getBuildCommand(defaultConfig, None, defaultDockerFilePath, defaultContextPath, additionalArgs)
    val expectedArgsString = additionalArgs.mkString(" ")

    command shouldBe s"docker build -f ${Escaper.shell(defaultDockerFilePath)} -t ${Escaper.shell(expectedImage)} $expectedArgsString ${Escaper.shell(defaultContextPath)}"
  }

  test("getBuildCommand should correctly escape paths and image names with spaces") {
    val engine = DockerEngine(
      image = "base/image:latest"
    )
    val dockerFilePathWithSpaces = "/tmp/My Dockerfile"
    val contextPathWithSpaces = "./my context path"
    val configWithSpaces = defaultConfig.copy(name = "my component with spaces", namespace = Some("test space"))
    val expectedImageWithSpaces = engine.getTargetIdentifier(configWithSpaces).toString

    val command = engine.getBuildCommand(configWithSpaces, None, dockerFilePathWithSpaces, contextPathWithSpaces)

    command shouldBe s"docker build -f ${Escaper.shell(dockerFilePathWithSpaces)} -t ${Escaper.shell(expectedImageWithSpaces)} ${Escaper.shell(contextPathWithSpaces)}"
  }

  test("getBuildCommand should throw ConfigException for unsupported builder type") {
    val engine = DockerEngine(
      image = "base/image:latest",
      builder = "unsupported_builder"
    )

    val ex = intercept[ConfigException] {
      engine.getBuildCommand(defaultConfig, None, defaultDockerFilePath, defaultContextPath)
    }
    ex.getMessage should include ("Unsupported builder: 'unsupported_builder'")
    ex.getMessage should include (s"Invalid 'builder: unsupported_builder' in engine configuration for component ${defaultConfig.name}.")
  }

  test("getBuildCommand should use engine id in tag if not 'docker'") {
    val customEngineId = "custom_docker"
    val engine = DockerEngine(
      id = customEngineId,
      image = "base/image:latest"
    )
    val expectedImage = engine.getTargetIdentifier(defaultConfig).toString
    val command = engine.getBuildCommand(defaultConfig, None, defaultDockerFilePath, defaultContextPath)

    command shouldBe s"docker build -f ${Escaper.shell(defaultDockerFilePath)} -t ${Escaper.shell(expectedImage)} ${Escaper.shell(defaultContextPath)}"
  }

  test("getBuildCommand should handle target_image and target_tag from DockerEngine config") {
    val engine = DockerEngine(
      image = "base/image:latest",
      target_image = Some("custom-image"),
      target_tag = Some("custom-tag"),
      target_organization = Some("my-org"),
      target_package = Some("my-pkg"),
      target_registry = Some("my.registry.com")
    )
    val expectedImage = engine.getTargetIdentifier(defaultConfig).toString

    val command = engine.getBuildCommand(defaultConfig, None, defaultDockerFilePath, defaultContextPath)

    command shouldBe s"docker build -f ${Escaper.shell(defaultDockerFilePath)} -t ${Escaper.shell(expectedImageManually)} ${Escaper.shell(defaultContextPath)}"
  }
}
