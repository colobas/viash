package io.viash.e2e.build

import io.viash._

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Paths, StandardCopyOption}
import io.viash.helpers.{IO, Exec, Logger}

import io.viash.config.Config

import scala.io.Source
import io.viash.helpers.data_structures._

class DockerSuite extends AnyFunSuite with BeforeAndAfterAll {
  Logger.UseColorOverride.value = Some(false)

  // which config to test
  private val configFile = getClass.getResource(s"/testbash/config.vsh.yaml").getPath

  private val temporaryFolder = IO.makeTemp("viash_tester")

  // parse config from file
  private val config = Config.read(configFile)

  // check whether executable was created
  private val executable = temporaryFolder.resolve(config.name).toFile

  // convert testbash
  test("viash can create an executable") {
    TestHelper.testMain(
      "build",
      configFile,
      "--engine", "docker",
      "--runner", "executable",
      "-o", temporaryFolder.toString,
    )

    assert(executable.exists)
    assert(executable.canExecute)
  }

  test("Check whether the executable can build the image", DockerTest) {
    val out = Exec.runCatch(
      Seq(executable.toString, "---setup", "build")
    )
    assert(out.exitValue == 0)
  }

  test("Check whether the executable can run", DockerTest) {
    Exec.run(
      Seq(executable.toString, "-h")
    )
  }

  test("Check whether particular keywords can be found in the usage", DockerTest) {
    val stdout =
      Exec.run(
        Seq(executable.toString, "--help")
      )

    val stripAll = (s : String) => s.replaceAll(raw"\s+", " ").trim

    config.allArguments.foreach(arg => {
      for (opt <- arg.alternatives; value <- opt)
        assert(stdout.contains(value))
      for (description <- arg.description) {
        assert(stripAll(stdout).contains(stripAll(description)))
      }
    })
  }

  test("Check whether output is correctly created", DockerTest) {
    val output = temporaryFolder.resolve("output.txt").toFile
    val log = temporaryFolder.resolve("log.txt").toFile

    val cmdOut = Exec.runCatch(
      Seq(
        executable.toString,
        executable.toString,
        "--real_number", "10.5",
        "--whole_number=10",
        "-s", "a string with a few spaces",
        "a", "b", "c",
        "--truth",
        "--output", output.getPath,
        "--log", log.getPath,
        "--optional", "foo",
        "--optional_with_default", "bar",
        "--multiple", "foo",
        "--multiple=bar",
        "d", "e", "f",
        "---cpus", "2",
        "---memory", "1gb"
      )
    )
    assert(cmdOut.exitValue == 0, "exit should be 0. stdout:\n" + cmdOut.output)

    assert(output.exists())
    assert(log.exists())

    val outputSrc = Source.fromFile(output)
    try {
      val outputLines = outputSrc.mkString
      assert(outputLines.contains(s"""input: |/viash_automount${executable.getPath}|"""))
      assert(outputLines.contains("""real_number: |10.5|"""))
      assert(outputLines.contains("""whole_number: |10|"""))
      assert(outputLines.contains("""s: |a string with a few spaces|"""))
      assert(outputLines.contains("""truth: |true|"""))
      assert(outputLines.contains(s"""output: |/viash_automount${output.getPath}|"""))
      assert(outputLines.contains(s"""log: |/viash_automount${log.getPath}|"""))
      assert(outputLines.contains("""optional: |foo|"""))
      assert(outputLines.contains("""optional_with_default: |bar|"""))
      assert(outputLines.contains("""multiple: |foo;bar|"""))
      assert(outputLines.contains("""multiple_pos: |a;b;c;d;e;f|"""))
      val regex = s"""meta_resources_dir: \\|.*${temporaryFolder}\\|""".r
      assert(regex.findFirstIn(outputLines).isDefined)
    } finally {
      outputSrc.close()
    }

    val logSrc = Source.fromFile(log)
    try {
      val logLines = logSrc.mkString
      assert(logLines.contains("INFO: Parsed input arguments"))
    } finally {
      logSrc.close()
    }
  }

  test("Alternative params", DockerTest) {
    val stdout =
      Exec.run(
        Seq(
          executable.toString,
          executable.toString,
          "--real_number", "123.456",
          "--whole_number", "789",
          "-s", "my$weird#string"
        )
      )

    assert(stdout.contains(s"""input: |/viash_automount${executable.getPath}|"""))
    assert(stdout.contains("""real_number: |123.456|"""))
    assert(stdout.contains("""whole_number: |789|"""))
    assert(stdout.contains("""s: |my$weird#string|"""))
    assert(stdout.contains("""truth: |false|"""))
    assert(stdout.contains("""optional: ||"""))
    assert(stdout.contains("""optional_with_default: |The default value.|"""))
    assert(stdout.contains("""multiple: ||"""))
    assert(stdout.contains("""multiple_pos: ||"""))
    val regex = s"""meta_resources_dir: \\|/viash_automount.*$temporaryFolder\\|""".r
    assert(regex.findFirstIn(stdout).isDefined)

    assert(stdout.contains("INFO: Parsed input arguments"))
  }

  test("Empty docker automount prefix", DockerTest) {
    val output = temporaryFolder.resolve("output.txt").toFile
    val log = temporaryFolder.resolve("log.txt").toFile

    val cmdOut = Exec.runCatch(
        Seq(
          executable.toString,
          executable.toString,
          "--real_number", "123.456",
          "--whole_number", "789",
          "-s", "my$weird#string",
          "--output", output.getPath,
          "--log", log.getPath,
        ),
        extraEnv = Seq(
          ("VIASH_DOCKER_AUTOMOUNT_PREFIX", "")
        )
      )

    assert(cmdOut.exitValue == 0, "exit should be 0. stdout:\n" + cmdOut.output)

    assert(output.exists())
    assert(log.exists())

    val outputSrc = Source.fromFile(output)
    try {
      val outputLines = outputSrc.mkString
      assert(outputLines.contains(s"""input: |${executable.getPath}|"""))
      assert(outputLines.contains("""real_number: |123.456|"""))
      assert(outputLines.contains("""whole_number: |789|"""))
      assert(outputLines.contains("""s: |my$weird#string|"""))
      assert(outputLines.contains("""truth: |false|"""))
      assert(outputLines.contains(s"""output: |/viash_automount${output.getPath}|"""))
      assert(outputLines.contains(s"""log: |/viash_automount${log.getPath}|"""))
      assert(outputLines.contains("""optional: ||"""))
      assert(outputLines.contains("""optional_with_default: |The default value.|"""))
      assert(outputLines.contains("""multiple: ||"""))
      assert(outputLines.contains("""multiple_pos: ||"""))
      val regex = s"""meta_resources_dir: \\|.*${temporaryFolder}\\|""".r
      assert(regex.findFirstIn(outputLines).isDefined)
    } finally {
      outputSrc.close()
    }

    val logSrc = Source.fromFile(log)
    try {
      val logLines = logSrc.mkString
      assert(logLines.contains("INFO: Parsed input arguments"))
    } finally {
      logSrc.close()
    }
  }

  test("Custom docker automount prefix", DockerTest) {
    val stdout =
      Exec.run(
        Seq(
          executable.toString,
          executable.toString,
          "--real_number", "123.456",
          "--whole_number", "789",
          "-s", "my$weird#string"
        ),
        extraEnv = Seq(
          ("VIASH_DOCKER_AUTOMOUNT_PREFIX", "/foobar")
        )
      )

    assert(stdout.contains(s"""input: |/foobar$executable|"""))
    assert(stdout.contains("""real_number: |123.456|"""))
    assert(stdout.contains("""whole_number: |789|"""))
    assert(stdout.contains("""s: |my$weird#string|"""))
    assert(stdout.contains("""truth: |false|"""))
    assert(stdout.contains("""optional: ||"""))
    assert(stdout.contains("""optional_with_default: |The default value.|"""))
    assert(stdout.contains("""multiple: ||"""))
    assert(stdout.contains("""multiple_pos: ||"""))
    val regex = s"""meta_resources_dir: \\|/foobar.*$temporaryFolder\\|""".r
    assert(regex.findFirstIn(stdout).isDefined)
    assert(!stdout.contains("/viash_automount"))

    assert(stdout.contains("INFO: Parsed input arguments"))
  }

  // Helper method for running build and basic execution tests
  private def runBuildAndExecuteTest(
    testName: String,
    configResourcePath: String,
    expectedOutputChecks: Seq[String => Boolean] = Seq.empty,
    setupCommand: String = "cachedbuild" // "build" or "cachedbuild"
  ): Unit = {
    val testConfigFile = getClass.getResource(configResourcePath).getPath
    assert(testConfigFile != null, s"$testName: Test config file '$configResourcePath' not found")

    val tempFolder = IO.makeTemp(s"viash_tester_${testName.replaceAll("[^a-zA-Z0-9]", "_")}")
    val componentConfig = Config.read(testConfigFile)
    val executableFile = tempFolder.resolve(componentConfig.name).toFile

    // Build the executable wrapper
    TestHelper.testMain(
      "build",
      testConfigFile,
      "--engine", "docker", // Still targeting the 'docker' platform type
      "--runner", "executable",
      "-o", tempFolder.toString
    )
    assert(executableFile.exists, s"$testName: Executable ${executableFile} should exist")
    assert(executableFile.canExecute, s"$testName: Executable ${executableFile} should be executable")

    // Build the Docker/OCI image
    val setupOut = Exec.runCatch(
      Seq(executableFile.toString, "---setup", setupCommand)
    )
    assert(setupOut.exitValue == 0, s"$testName: Image setup failed. Stdout:\n${setupOut.output}\nStderr:\n${setupOut.error}")

    // Run the component
    val runArgs = Seq(
      executableFile.toString,
      "---", // Separator for platform args if any, then component args
      "--real_number", "3.14",
      "--whole_number", "42",
      "-s", s"hello from $testName"
    )
    val runOutput = Exec.runCatch(runArgs)

    assert(runOutput.exitValue == 0, s"$testName: Running component failed. Stdout:\n${runOutput.output}\nStderr:\n${runOutput.error}")
    assert(runOutput.output.contains("real_number: |3.14|"), s"$testName: Output did not contain expected real_number")
    assert(runOutput.output.contains("whole_number: |42|"), s"$testName: Output did not contain expected whole_number")
    assert(runOutput.output.contains(s"s: |hello from $testName|"), s"$testName: Output did not contain expected string")

    expectedOutputChecks.foreach(check => assert(check(runOutput.output), s"$testName: Custom output check failed."))

    // Clean up
    IO.deleteRecursively(tempFolder)
  }

  test("Buildah: viash can build an image using buildah and run the component", DockerTest) {
    runBuildAndExecuteTest(
      testName = "BuildahDefault",
      configResourcePath = "/testbash_buildah/config.vsh.yaml"
    )
  }

  test("DockerOverrideExplicit: viash can build with explicit 'docker' override", DockerTest) {
    runBuildAndExecuteTest(
      testName = "DockerOverrideExplicit",
      configResourcePath = "/testbash_docker_override_explicit/config.vsh.yaml"
    )
  }

  test("DockerOverridePodman: viash can build with 'podman' override for docker builder", DockerTest) {
    // This test assumes Podman is installed and behaves like Docker for 'build'
    // It might require specific setup on the CI or local test environment if Podman is not available.
    // Check if podman is available, skip if not.
    val podmanAvailable = Exec.runCatch(Seq("podman", "--version")).exitValue == 0
    if (podmanAvailable) {
      runBuildAndExecuteTest(
        testName = "DockerOverridePodman",
        configResourcePath = "/testbash_docker_override_podman/config.vsh.yaml",
        // Podman might output slightly different image IDs or messages during build,
        // but the component execution should be the same.
        // No specific output checks beyond standard component output needed here unless debugging.
      )
    } else {
      pending // Mark test as pending if podman is not available
      info("Skipping DockerOverridePodman test as Podman is not available in PATH.")
    }
  }

  test("BuildahOverrideCustomApptainer: viash can build with custom apptainer command for buildah", DockerTest) {
    runBuildAndExecuteTest(
      testName = "BuildahOverrideCustomApptainer",
      configResourcePath = "/testbash_buildah_override_custom_apptainer/config.vsh.yaml"
      // We expect this to use a different buildah image version if the override is effective.
      // The component output itself should remain standard.
    )
  }

  override def afterAll(): Unit = {
    IO.deleteRecursively(temporaryFolder)
  }
}
