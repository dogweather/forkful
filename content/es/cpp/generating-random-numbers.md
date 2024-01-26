---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:48:53.878678-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios es obtener valores que no siguen un patrón predecible. Los programadores usan esto para simular incertidumbre, probar algoritmos, y para juegos o aplicaciones que dependen de la aleatoriedad.

## Cómo hacerlo:

Para generar números aleatorios en C++, puedes usar la biblioteca estándar `<random>`. Aquí hay un ejemplo:

```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd;  // Obtener un número aleatorio del hardware si es posible
    std::mt19937 gen(rd()); // Inicializar el generador de números aleatorios
    std::uniform_int_distribution<> distrib(1, 100); // Definir rango de los números aleatorios

    // Generar y mostrar un número aleatorio
    std::cout << "Número aleatorio: " << distrib(gen) << std::endl;

    return 0;
}
```

Ejemplo de salida:
```
Número aleatorio: 42
```

## Profundización:

Históricamente, los números aleatorios se generaban físicamente (como lanzar un dado), pero esto no es práctico en computación. Antes de `<random>`, se usaba `rand()` de `<cstdlib>`, que es menos avanzado y predecible.

Las alternativas modernas de C++ incluyen:

- `<random>` con generadores como `std::mt19937` (Mersenne Twister) y distribuciones como `std::uniform_int_distribution` para personalizar rangos.
- Librerías externas (por ejemplo, Boost.Random) ofrecen más funciones y adaptabilidad.

Detalles de implementación:

- `std::random_device` intenta usar un dispositivo de hardware para obtener aleatoriedad real, pero puede no ser completamente no determinista en todas las plataformas.
- Las semillas (`seed`) ayudan a iniciar el estado interno de un generador de números. Usar una fuente como `std::random_device` mejora la aleatoriedad comparado con una semilla fija.
- La distribución transforma la salida del generador para que se ajuste a ciertos rangos o distribuciones estadísticas.

## Véase También:

- Documentación de la Biblioteca estándar de C++ para `<random>`: http://www.cplusplus.com/reference/random/
- Artículo sobre generación de números pseudoaleatorios en C++: https://en.cppreference.com/w/cpp/numeric/random
- Detalles del algoritmo Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister
