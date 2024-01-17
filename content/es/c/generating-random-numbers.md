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

## ¿Qué & Por qué?
Generar números aleatorios es una técnica común en la programación en la que se crean valores aleatorios para utilizar en un programa. Los programadores lo utilizan para crear resultados impredecibles o para seleccionar una opción al azar dentro de un conjunto.

## Cómo hacerlo:
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Para generar un número aleatorio entre 1 y 10:
    // 1. Inicializar la semilla utilizando la hora actual
    // 2. Llamar a la función rand() y generar un número entre 0 y RAND_MAX
    // 3. Dividir el número generado por RAND_MAX y multiplicar por 10
    // 4. Redondear el resultado utilizando la función round()
    // 5. Imprimir el resultado
    srand(time(NULL));
    int numero = round(rand() * 10.0 / RAND_MAX);
    printf("Número aleatorio: %d\n", numero);
    return 0;
}
```

Output:
```
Número aleatorio: 4
```

## Profundizando:
La generación de números aleatorios ha sido un desafío en la programación desde sus inicios. Antes de la invención de los ordenadores electrónicos, se utilizaban métodos como tirar dados o barajar cartas para obtener resultados aleatorios. Sin embargo, con la llegada de los ordenadores, se desarrollaron algoritmos y funciones matemáticas para generar números aleatorios de manera eficiente.

Algunas alternativas a la función `rand()` incluyen el uso de generadores de números pseudoaleatorios (PRNG), que aunque no generan números verdaderamente aleatorios, pueden ser útiles en ciertos casos.

La implementación del método `rand()` en C utiliza un generador congruencial lineal, que utiliza una fórmula matemática para producir una secuencia de números aparentemente aleatorios. Esta secuencia se puede repetir utilizando la misma semilla, lo que significa que el resultado no es verdaderamente aleatorio.

## Ver también:
- [Tutorial sobre generación de números aleatorios en C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Más información sobre generadores de números pseudoaleatorios en C](https://www.geeksforgeeks.org/generating-random-number-range-c/)
- [Explicación detallada sobre el funcionamiento de la función `rand()`](https://www.cs.hmc.edu/~geoff/classes/hmc.cs070.200101/homework10/rand_jenkins.pdf)