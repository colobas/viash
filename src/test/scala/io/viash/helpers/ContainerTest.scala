package io.viash.helpers

import org.scalatest.funsuite.AnyFunSuite
import io.viash.config._

class ContainerTest extends AnyFunSuite {
  test("ContainerImageInfo toString for Docker") {
    val dockerImage = ContainerImageInfo(
      name = "ubuntu",
      tag = "20.04",
      registry = Some("docker.io"),
      organization = Some("library"),
      containerType = "docker"
    )

    assert(dockerImage.toString === "docker.io/library/ubuntu:20.04")
  }

  test("ContainerImageInfo toString for Apptainer") {
    val apptainerImage = ContainerImageInfo(
      name = "myimage.sif",
      tag = "latest", // This should be ignored for Apptainer
      registry = Some("ignored"), // This should be ignored for Apptainer
      organization = Some("ignored"), // This should be ignored for Apptainer
      containerType = "apptainer"
    )

    assert(apptainerImage.toString === "myimage.sif")
  }

  test("ContainerImageInfo toMap") {
    val dockerImage = ContainerImageInfo(
      name = "ubuntu",
      tag = "20.04",
      registry = Some("docker.io"),
      organization = Some("library"),
      containerType = "docker"
    )

    val map = dockerImage.toMap
    assert(map("registry") === "docker.io")
    assert(map("image") === "library/ubuntu")
    assert(map("tag") === "20.04")
    assert(map("containerType") === "docker")
  }

  test("Container.getImageInfo with Docker type") {
    val config = Config(
      name = "test_component",
      version = Some("1.0.0")
    )

    val imageInfo = Container.getImageInfo(
      config = Some(config),
      namespaceSeparator = Some("/"),
      containerType = "docker"
    )

    assert(imageInfo.name === "test_component")
    assert(imageInfo.tag === "1.0.0")
    assert(imageInfo.containerType === "docker")
  }

  test("Container.getImageInfo with Apptainer type") {
    val config = Config(
      name = "test_component",
      version = Some("1.0.0")
    )

    val imageInfo = Container.getImageInfo(
      config = Some(config),
      namespaceSeparator = Some("/"),
      containerType = "apptainer"
    )

    assert(imageInfo.name === "test_component")
    assert(imageInfo.tag === "1.0.0")
    assert(imageInfo.containerType === "apptainer")
  }

  test("Container.getImageInfo with explicit name containing tag") {
    val imageInfo = Container.getImageInfo(
      name = Some("ubuntu:20.04"),
      containerType = "docker"
    )

    assert(imageInfo.name === "ubuntu")
    assert(imageInfo.tag === "20.04")
  }

  test("Container.getImageInfo with engine ID suffix") {
    val config = Config(
      name = "test_component",
      version = Some("1.0.0")
    )

    val imageInfo = Container.getImageInfo(
      config = Some(config),
      namespaceSeparator = Some("/"),
      engineId = Some("custom"),
      containerType = "docker"
    )

    assert(imageInfo.name === "test_component")
    assert(imageInfo.tag === "1.0.0-custom")
  }

  test("Container.getImageInfo with namespace") {
    val config = Config(
      name = "test_component",
      namespace = Some("my_namespace"),
      version = Some("1.0.0")
    )

    val imageInfo = Container.getImageInfo(
      config = Some(config),
      namespaceSeparator = Some("/"),
      containerType = "docker"
    )

    assert(imageInfo.name === "my_namespace/test_component")
    assert(imageInfo.tag === "1.0.0")
  }
}
