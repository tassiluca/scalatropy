<div align="center">

<h1>ScalaTropy</h1>
<p><strong>Typed multiparty programming in Scala.</strong></p>

ScalaTropy is a Scala library for describing distributed applications as multiparty programs. Define the choreography once, project it onto typed peers, and run those peers over configured network connections.

[![Build and check](https://github.com/tassiluca/ScalaTropy/actions/workflows/ci.yml/badge.svg)](https://github.com/tassiluca/ScalaTropy/actions/workflows/ci.yml)
[![Latest release](https://img.shields.io/github/v/release/tassiluca/ScalaTropy)](https://github.com/tassiluca/ScalaTropy/releases)
[![License](https://img.shields.io/github/license/tassiluca/ScalaTropy)](https://github.com/tassiluca/ScalaTropy/blob/master/LICENSE)
[![Scala](https://img.shields.io/badge/Scala-3.9.0-red)](https://www.scala-lang.org/)
[![GitHub stars](https://img.shields.io/github/stars/tassiluca/ScalaTropy)](https://github.com/tassiluca/ScalaTropy/stargazers)

</div>

## What it provides

- A DSL for describing communication and coordination between typed peers.
- Endpoint projection to execute a multiparty program as local peer behaviors.
- Network abstractions with MQTT, WebSocket, and in-memory implementations.
- Examples ranging from simple message exchanges to replicated services.

## Project structure

- [`core/`](core/): the multiparty language, peer and deployment abstractions, and shared APIs.
- [`distributed/`](distributed/): network implementations for running peers on distributed transports.
- [`examples/`](examples/): runnable programs demonstrating ScalaTropy's features.

## Quick start

Start the local MQTT broker:

```bash
docker compose up mosquitto
```

In another terminal, build the examples and run the PingPong program:

```bash
./mill examples.assembly
java -cp out/examples/assembly.dest/out.jar it.unibo.pslab.PingPongApp
```

Replace `it.unibo.pslab.PingPongApp` with another launcher from the examples table to run a different program. The examples use the broker at `localhost:1883`.

MQTT peers wait for discovery before starting the choreography. The default initial wait window is 5 seconds; configure it when creating the network if your deployment needs more time:

```scala
val mqttConfig = Configuration(appId = "my-app-id", initialWaitWindow = 15.seconds)
```

## Evaluation

Reproduce the message-overhead analysis for broadcast and selective communication with:

```bash
docker compose up evaluation
```

The experiment runs take approximately 3 minutes. The resulting plot and raw data are saved in `evaluation/`.
