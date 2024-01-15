---
title:                "Generando números aleatorios"
html_title:           "C++: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios?

Generar números aleatorios es útil en muchas aplicaciones de programación, como juegos, simulaciones y criptografía. También puede ser utilizado para probar y depurar código.

## Cómo hacerlo

Para generar números aleatorios en C++, primero debemos incluir la biblioteca "cstdlib" y utilizar la función "rand()" que nos provee. Luego, podemos utilizar operaciones matemáticas para limitar el rango de los números generados.

Por ejemplo, si queremos generar un número entre 1 y 10, podemos utilizar el módulo (operador %) para limitar el rango, como se muestra en el siguiente código:

```C++
#include <cstdlib>
#include <iostream>

int main() {
  int numero_random = (rand() % 10) + 1; // genera un número entre 1 y 10
  std::cout << "El número aleatorio es: " << numero_random << std::endl;
  return 0;
}
```

Este código generará un número aleatorio cada vez que se ejecute.

## Profundizando en la generación de números aleatorios

La función "rand()" utiliza un algoritmo para generar números pseudoaleatorios, lo que significa que los números no son realmente aleatorios, sino que siguen un patrón predecible. Por esta razón, es importante utilizar una semilla (seed) al principio del programa para generar una secuencia diferente cada vez.

Además, en lugar de utilizar solamente la función "rand()", se pueden utilizar otras funciones de la biblioteca "cstdlib" como "srand()" y "time()" para generar números más aleatorios.

## Véase también

- [Más sobre la función rand() en C++ (en inglés)](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Ejemplos prácticos de generación de números aleatorios en C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Más información sobre algoritmos de generación de números aleatorios (en inglés)](https://www.sciencedirect.com/science/article/abs/pii/0377042781900841)