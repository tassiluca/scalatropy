package it.unibo.pslab.smartgarden

import it.unibo.pslab.network.{ MQTT, WebSocket }
import it.unibo.pslab.peers.Peers.*

object SmartGardenArchitecture:

  type Server <: { type Tie <: via[MQTT toMultiple Device] & via[WebSocket toSingle Dashboard] }
  type Dashboard <: { type Tie <: via[WebSocket toSingle Server] }

  type Device <: { type Tie <: via[MQTT toSingle Server] }

  type Robot <: Device
  type LawnMower <: Robot

  type Sensor <: Device
  type SoilMoistureSensor <: Sensor
