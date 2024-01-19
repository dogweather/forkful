---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Generar números aleatorios es crear números que no siguen un patrón predecible. Los programadores hacen esto para simular eventos aleatorios en sus programas, como tirar los dados en un juego de mesa virtual.

## ¿Cómo hacerlo?

En C++ (versión actual), tenemos una biblioteca llamada `<random>` para generar números aleatorios. A continuación, te muestro un simple código para generar un número aleatorio entre 1 y 6.

```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd; 
    std::mt19937 gen(rd()); 
    std::uniform_int_distribution<> dis(1, 6);

    std::cout << dis(gen) << '\n'; 

    return 0;
}
```

Una posible salida puede ser:

```terminal
4
```

## Profundización

Históricamente, los números pseudoaleatorios se generaban mediante una 'semilla' y un algoritmo de generación bastante predecible. El advenimiento de `<random>` en C++11 trajo técnicas modernas y más fiables para generar números aleatorios.

Existen alternativas a `<random>`, como la función `rand()` de C, pero tiene problemas, como un bajo grado de aleatoriedad y un límite superior arcaico.

La generación de números aleatorios con `<random>` implica la creación de un dispositivo aleatorio (`std::random_device`), un generador (\`std::mt19937` en este caso, que es un tipo de generador Mersenne Twister), y una distribución (\`std::uniform_int_distribution<>\`). El dispositivo aleatorio se usa para sembrar el generador, que a su vez alimenta a la distribución para generar números aleatorios.

## Ver también

Para más detalles sobre `<random>`, puedes revisar la documentación en cppreference.com: <https://en.cppreference.com/w/cpp/numeric/random>

Para entender más acerca de los números aleatorios, consulta el siguiente enlace de Wikipedia: <https://es.wikipedia.org/wiki/N%C3%BAmero_aleatorio>

¡Buena suerte con tu generación de números aleatorios!