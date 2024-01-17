---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?:
Generar números aleatorios es un proceso importante en la programación, ya que permite a los programadores crear aplicaciones más dinámicas y variadas. Esto se logra mediante la creación de valores numéricos aleatorios que pueden ser utilizados en diferentes situaciones, como en juegos, simulaciones, o en la solución de problemas matemáticos.

## Cómo hacerlo:
Para generar números aleatorios en Arduino, se puede utilizar la función `random(min, max)`, donde `min` y `max` son los valores mínimo y máximo del rango en el que se desea generar el número. Por ejemplo:

```
// Generar un número aleatorio entre 1 y 100
int numero = random(1, 100);
Serial.println(numero); // Imprime el número generado en el monitor serial
```

También se puede utilizar la función `randomSeed(seed)` para inicializar la secuencia de números aleatorios. Esto es útil si se desea una secuencia diferente de números en cada ejecución del programa. Por ejemplo:

```
// Generar un número aleatorio con una semilla (seed) de 123
randomSeed(123);
int numero = random(1, 100);
Serial.println(numero); // Imprime el número generado en el monitor serial
```

## Profundizando:
La generación de números aleatorios es un tema que ha sido estudiado por muchos matemáticos y programadores a lo largo de la historia. Además de la función `random()` en Arduino, existen otras formas de generar números aleatorios, como por ejemplo utilizando algoritmos basados en la hora actual del sistema.

También es importante tener en cuenta que los números generados utilizando la función `random()` en Arduino no son realmente aleatorios, sino pseudoaleatorios. Esto significa que, aunque los números parezcan aleatorios, en realidad se generan a partir de una fórmula matemática determinada.

En cuanto a la implementación de la función `random()` en Arduino, esta se basa en la función `rand()` de C, que a su vez utiliza el algoritmo "Linear Congruential Generator" (LCG). Este algoritmo tiene sus limitaciones y puede repetir secuencias de números después de un cierto número de iteraciones.

## También puedes ver:
Si quieres profundizar más en el tema de la generación de números aleatorios en Arduino, puedes consultar la documentación oficial de Arduino sobre la función `random()` y el algoritmo LCG. También puedes investigar sobre otros métodos de generación de números aleatorios como el algoritmo Mersenne Twister o el hardware RNG (Random Number Generator).