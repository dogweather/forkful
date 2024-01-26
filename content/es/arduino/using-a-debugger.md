---
title:                "Usando un depurador"
date:                  2024-01-26T03:47:03.698352-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/using-a-debugger.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Un depurador es una herramienta que te ayuda a aplastar errores en tu código permitiéndote pausar, indagar y descubrir qué está pasando realmente bajo el capó. Los programadores utilizan depuradores para avanzar paso a paso a través de su código, inspeccionar variables y entender dónde las cosas podrían estar yendo mal.

## Cómo:

Con el IDE de Arduino, puedes usar impresiones Serial para depurar, pero es un poco como usar una linterna para explorar una cueva. Para una depuración real, podrías querer subir de nivel tu juego con algo como el depurador Atmel-ICE, que se integra con el entorno de Arduino. Aquí tienes un sabor de la pseudo-depuración usando Serial:

```Arduino
void setup() {
  Serial.begin(9600);
}
void loop() {
  int sensorValue = analogRead(A0);
  Serial.print("Valor del Sensor: ");
  Serial.println(sensorValue);
  // Imagina que esperas 512 aquí, pero obtienes 0.
  // Momento de inspeccionar la conexión del sensor
  delay(1000); // Espera un segundo antes de leer de nuevo
}
```
Ejecuta esto con el Monitor Serial abierto, y verás lo que tu sensor expulsa en tiempo real.

## Inmersión Profunda

Antes de los depuradores, era el mundo de las declaraciones de impresión: solo podías adivinar qué estaba pasando imprimiendo todo. La depuración con impresiones sigue siendo común, especialmente en entornos más simples o en hardware restringido como el Arduino.

Las alternativas a los emuladores en circuito como Atmel-ICE incluyen herramientas de depuración de software como `avr-gdb`. Puedes emparejarlo con `avarice` para crear un puente entre GDB y tu hardware, lo cual es súper útil para una depuración más avanzada directamente en el chip.

Usando un depurador, puedes establecer puntos de interrupción para detener la ejecución en ciertos puntos. Puedes avanzar a través de tu código línea por línea, inspeccionar memoria, registros y variables. Esto te permite precisar problemas en lugar de disparar en la oscuridad. Al implementar un depurador, asegúrate de que tu entorno esté configurado correctamente: las versiones incompatibles o las herramientas mal configuradas pueden llevar a frustraciones.

## Ver También

¿Listo para profundizar? Sumérgete en estos:
- La guía de depuración de Arduino en [Depuración de Arduino](https://www.arduino.cc/en/Guide/Environment#toc7)
- El manual de referencia de AVR Libc para configurar avr-gdb: [Página Principal de AVR Libc](http://www.nongnu.org/avr-libc/)
- Una inmersión más profunda en el uso de Atmel-ICE: [Usando Atmel-ICE para la Programación AVR en Atmel Studio](https://microchipdeveloper.com/atmel-ice:atmel-ice)