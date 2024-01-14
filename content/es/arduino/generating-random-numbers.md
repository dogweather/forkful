---
title:                "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios con Arduino?

Generar números aleatorios es una forma útil y divertida de incorporar una variable dinámica a nuestros proyectos con Arduino. Con la función de generación de números aleatorios, podemos crear programas más interesantes y sorprendentes.

## Cómo hacerlo

Para generar números aleatorios con Arduino, utilizaremos la función `random(min, max)` para generar un número aleatorio entre un valor mínimo y uno máximo. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, nuestro código se vería así:

```Arduino
// Generar un número aleatorio entre 1 y 10
int numero = random(1, 10);
Serial.println(numero); // Imprimirá un número aleatorio entre 1 y 10
```

También podemos generar un número aleatorio entre 0 y 255, que es el rango máximo para variables de tipo `int` en Arduino. Podemos usar esto para crear efectos de luz o sonido aleatorios en nuestros proyectos.

```Arduino
// Generar un número aleatorio entre 0 y 255
int numero = random(0, 255);
Serial.println(numero); // Imprimirá un número aleatorio entre 0 y 255
```

## Profundizando

La función `random()` en Arduino utiliza un algoritmo llamado generador de números pseudoaleatorios basado en el tiempo actual del microcontrolador. Esto significa que los números generaros no son realmente aleatorios, sino que siguen un patrón predecible. Sin embargo, para la mayoría de aplicaciones, esto es suficiente.

Si necesitamos generar números más aleatorios, podemos usar un módulo de sensor externo, como un sensor de luz o de temperatura, para leer valores aleatorios y utilizarlos en nuestra función `random()`. También podemos usar una fuente externa de ruido, como el ruido eléctrico, para generar números más impredecibles.

## Ver también

- Tutorial de Arduino sobre la función `random()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Video tutorial en español sobre cómo generar números aleatorios con Arduino: https://www.youtube.com/watch?v=pPWpRRWvjcg
- Proyecto de ejemplo de Arduino utilizando números aleatorios: https://create.arduino.cc/projecthub/XmlElement/random-color-led-1cb04b