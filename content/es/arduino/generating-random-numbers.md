---
title:                "Arduino: Generando números aleatorios"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una tarea común en la programación, ya sea para crear juegos, realizar pruebas o para fines estadísticos. En este blog post, aprenderás cómo generar números aleatorios en Arduino.

## Cómo hacerlo

Para generar números aleatorios en Arduino, utilizaremos la función `random()` que devuelve un número entero aleatorio dentro de un rango especificado. Por ejemplo, si queremos generar un número aleatorio entre 0 y 10, usaremos `random(0,10)`.

```Arduino
int randomNum = random(0,10); // generando un número aleatorio entre 0 y 10
Serial.println(randomNum); // imprimiendo el número aleatorio en el monitor serial
```

La función `random()` se basa en una semilla (seed, en inglés) que es un número utilizado para iniciar el generador de números aleatorios. Siempre generará la misma secuencia de números a menos que se cambie la semilla. Para cambiar la semilla, podemos utilizar la función `randomSeed()` y pasarle un número, por ejemplo, basado en la lectura de un pin analógico.

```Arduino
randomSeed(analogRead(A0)); // cambiando la semilla basándonos en la lectura de un pin analógico
```

La función `random()` también se puede usar para generar números aleatorios en punto flotante (números con decimales). Simplemente utilizamos `random()` sin especificar ningún rango y luego lo dividimos por un número para obtener un decimal.

```Arduino
float randomDecimal = random() / 10.0; // generando un número aleatorio con decimales entre 0 y 1
Serial.println(randomDecimal); // imprimiendo el número aleatorio en el monitor serial
```

## Profundizando

La función `random()` en Arduino utiliza el generador de números pseudoaleatorios (PRNG, en inglés) llamado "linear congruential" que tiene una fórmula matemática para generar los números aleatorios en secuencia. Debido a que es un PRNG, los números generados no son verdaderamente aleatorios, pero son suficientes para la mayoría de aplicaciones.

Si necesitas números verdaderamente aleatorios, puedes utilizar un módulo externo de hardware, como un sensor de ruido, para generar una semilla aleatoria y luego utilizar la función `randomSeed()` con esa semilla.

## Ver también

- Documentación de `random()` en [arduino.cc](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/).
- Tutorial de generación de números aleatorios de [Adafruit](https://learn.adafruit.com/circuitpython-random-number-generation/overview).