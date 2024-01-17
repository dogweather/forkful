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

## Qué y Por qué?
La generación de números aleatorios es una técnica utilizada por los programadores para obtener valores aleatorios en sus aplicaciones. Esto puede ser útil en juegos, simulaciones y algoritmos que requieren una entrada aleatoria para probar su eficacia.

## Cómo:
```
#include <iostream>
#include <cstdlib>
using namespace std;

int main() {
    // Generando un número aleatorio entre 1 y 10
    int random = rand() % 10 + 1;
    cout << "El número aleatorio es: " << random << endl;
    return 0;
}
```
Salida de ejemplo:
```
El número aleatorio es: 7
```

## Profundizando:
La generación de números aleatorios se ha utilizado desde los primeros días de la programación y ha evolucionado a lo largo de los años. Además del uso de la función `rand()` en C++, también existen otras bibliotecas y algoritmos para generar números aleatorios. Sin embargo, los resultados de estos métodos no siempre son verdaderamente aleatorios y pueden predecirse en algunos casos.

## Ver también:
- [Documentación de la función `rand()` en cplusplus.com](http://www.cplusplus.com/reference/cstdlib/rand/)
- [Generación de números aleatorios en C++ - Programiz](https://www.programiz.com/cpp-programming/random-numbers)