---
title:                "C++: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios es importante en la programación de C++

Cuando se programa en C++, a menudo nos encontramos con la necesidad de generar números aleatorios. Ya sea para crear un juego, realizar pruebas de rendimiento o cualquier otra tarea, la generación de números aleatorios es una habilidad esencial para cualquier programador.

## Cómo generar números aleatorios en C++

En C++, podemos utilizar la función `rand()` para generar números aleatorios de forma sencilla. Esta función nos devuelve un número entero aleatorio dentro de un rango determinado.

Veamos un ejemplo en código:
```C++
// Incluimos la librería necesaria
#include <cstdlib>

// Generamos un número aleatorio entre 1 y 10
int numero = rand() % 10 + 1;

// Mostramos el número por pantalla
std::cout << "El número aleatorio es: " << numero << std::endl;
```
Si ejecutamos este código varias veces, veremos cómo el número aleatorio generado cambia cada vez.

## Profundizando en la generación de números aleatorios

Para generar números aleatorios más precisos, podemos utilizar la función `srand()` para establecer una semilla antes de utilizar la función `rand()`. Esto nos permite tener un mayor control sobre los números que se generan.

También es importante recordar que los números aleatorios no son verdaderamente aleatorios, sino que se generan a partir de un algoritmo matemático. Por ello, es importante elegir una buena semilla y evitar patrones que puedan afectar a la "aleatoriedad" de los números generados.

## Ver también

- [Random number generation in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [C++ Reference for rand()](https://www.cplusplus.com/reference/cstdlib/rand/)
- [C++ Reference for srand()](https://www.cplusplus.com/reference/cstdlib/srand/)