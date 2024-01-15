---
title:                "Generando números aleatorios"
html_title:           "C: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

La generación de números aleatorios es una herramienta fundamental en la programación, ya que permite crear programas que toman decisiones de forma no determinista. Además, es útil en la creación de juegos y en la realización de pruebas aleatorias.

## Cómo

La función `rand()` en C nos permite generar números aleatorios. Esta función devuelve un número entero aleatorio dentro de un rango determinado. Pero antes de usarla, es importante inicializar la semilla utilizando `srand()` con un valor único, como por ejemplo `time(NULL)`. Veamos un ejemplo de cómo generar un número aleatorio entre 1 y 10.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  int r = (rand() % 10) + 1;
  printf("El número aleatorio es: %d\n", r);
  return 0;
}
```
La salida puede variar cada vez que se ejecute el programa, ya que los números aleatorios generados dependen de la semilla.

## Deep Dive

La generación de números aleatorios en C está basada en un algoritmo llamado "Linear Congruential Generator" (LCG). Este algoritmo utiliza una fórmula matemática para generar números pseudoaleatorios. Para obtener un resultado más aleatorio, se pueden mezclar con otros números o utilizar más bits en la operación. Es importante entender que los números generados no son completamente aleatorios y pueden repetirse después de cierta cantidad de iteraciones.

## Ver también

- [Función rand en cplusplus.com](http://www.cplusplus.com/reference/cstdlib/rand/)
- [Uso de srand en GeeksforGeeks](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Artículo sobre generación de números aleatorios en C](https://i.stack.imgur.com/ghfpZ.png)