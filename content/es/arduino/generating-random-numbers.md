---
title:                "Generación de números aleatorios"
html_title:           "Arduino: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Generar números aleatorios es una tarea común en muchos proyectos de Arduino. Ya sea en juegos, simulaciones o simplemente para crear un elemento de sorpresa, contar con la capacidad de generar números aleatorios puede ser muy útil. En este artículo, te explicaremos cómo puedes hacerlo utilizando la última versión de Arduino.

## Cómo Hacerlo

Primero, debes incluir la librería "```ArduinoRandom.h```" en tu sketch de Arduino. Luego, puedes utilizar la función "random()" para generar un número aleatorio. Aquí tienes un ejemplo de código que genera un número aleatorio entre 0 y 10 y lo imprime en el Monitor Serial:

```Arduino
#include "ArduinoRandom.h" // Incluimos la librería

void setup() {
  Serial.begin(9600); // Iniciamos la comunicación serial
}

void loop() {
  int num = random(0, 10); // Generamos un número aleatorio entre 0 y 10
  Serial.println(num); // Imprimimos el número en el Monitor Serial
  delay(1000); // Esperamos 1 segundo antes de volver a generar otro número
}
```

Si ejecutas este código, verás que se imprimen diferentes números aleatorios cada segundo en el Monitor Serial.

## Inmersión Profunda

La función "random()" en realidad utiliza un algoritmo llamado "Mersenne Twister" para generar los números aleatorios. Este algoritmo es un estándar en la industria de la computación para generar números pseudo-aleatorios. 

Además, la función "random()" tiene una variante llamada "randomSeed()" que permite inicializar la secuencia de números aleatorios con un valor específico. Esto puede ser útil si quieres generar la misma secuencia de números en diferentes ejecuciones de tu programa.

## Ver También

- Documentación oficial sobre cómo generar números aleatorios en Arduino: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Tutorial sobre cómo generar números aleatorios en Arduino: https://howtomechatronics.com/tutorials/arduino/how-to-generate-random-numbers-arduino-tutorial/