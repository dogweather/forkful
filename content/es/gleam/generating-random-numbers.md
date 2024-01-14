---
title:                "Gleam: Generación de números aleatorios"
simple_title:         "Generación de números aleatorios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

¡Hola a todos los lectores de Gleam! Si eres nuevo en el mundo de la programación, es probable que hayas escuchado sobre la generación de números aleatorios. ¿Pero alguna vez te has preguntado por qué es importante aprender a generar números aleatorios y cómo puedes hacerlo en Gleam? ¡Sigue leyendo para descubrirlo!

## Por qué

La generación de números aleatorios es una habilidad muy útil en programación. Puedes usarla para simular eventos aleatorios en juegos, aplicaciones de lotería, pruebas unitarias y muchas otras áreas de programación. Además, también es útil para crear datos de prueba aleatorios para probar tu código, especialmente cuando se trata de algoritmos de ordenamiento o búsqueda.

## Cómo hacerlo

En Gleam, generar números aleatorios es muy sencillo. Simplemente necesitas la función incorporada "random.int(min, max)" que acepta dos argumentos: el número mínimo y el número máximo que quieres generar. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, podemos hacerlo de la siguiente manera:

```
Gleam
import random

let numero = random.int(1, 10)

io.println(numero) // output: un número aleatorio entre 1 y 10
```

También puedes utilizar la función "random.float(min, max)" si necesitas generar números aleatorios con decimales. Además, puedes especificar la semilla utilizando la función "random.seed(seed)" para obtener resultados consistentes cada vez que ejecutes tu código.

## Profundizando

Si quieres profundizar en el tema de la generación de números aleatorios en Gleam, es importante entender cómo funciona el algoritmo detrás de la función "random.int(min, max)". Gleam utiliza el generador de números aleatorios Mersenne Twister, que es uno de los generadores de números aleatorios más utilizados y confiables en la programación. Este algoritmo utiliza una semilla y una fórmula matemática para generar una secuencia de números aparentemente aleatorios.

Sin embargo, es importante tener en cuenta que la generación de números aleatorios en programación no es completamente aleatoria, sino pseudoaleatoria. Esto significa que los números son generados de manera determinística a partir de una semilla, por lo que si se utiliza la misma semilla, se obtendrá la misma secuencia de números. Por lo tanto, es importante elegir una semilla cuidadosamente para obtener una buena distribución de números aleatoreos.

## Ver también
- [Documentación oficial de Gleam sobre generación de números aleatorios](https://gleam.run/modules/gleam/random.html)
- [Información sobre el generador de números aleatorios Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Ejemplos de uso de generación de números aleatorios en Gleam](https://github.com/gleam-lang/gleam/blob/master/tests/random_test.test)