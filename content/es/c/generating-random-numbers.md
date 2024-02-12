---
title:                "Generando números aleatorios"
aliases:
- es/c/generating-random-numbers.md
date:                  2024-02-03T17:57:05.523122-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generando números aleatorios"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/generating-random-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios en C implica crear valores que son impredecibles y siguen una distribución específica, como uniforme o normal. Esta capacidad es crucial para aplicaciones que van desde simulaciones y juegos hasta operaciones criptográficas, donde la imprevisibilidad o la simulación de aleatoriedad del mundo real es esencial.

## Cómo hacerlo:

En C, los números aleatorios se pueden generar usando la función `rand()`, que es parte de la biblioteca estándar de C `<stdlib.h>`. Por defecto, `rand()` produce números pseudoaleatorios en el rango de 0 a `RAND_MAX` (una constante definida en `<stdlib.h>`). Para tener más control sobre el rango, los programadores pueden manipular la salida de `rand()`.

Aquí hay un ejemplo simple de generación de un número aleatorio entre 0 y 99:

```c
#include <stdio.h>
#include <stdlib.h> // Para rand() y srand()
#include <time.h>   // Para time()

int main() {
    // Sembrar el generador de números aleatorios
    srand((unsigned) time(NULL));

    // Generar un número aleatorio entre 0 y 99
    int randomNumber = rand() % 100;

    printf("Número Aleatorio: %d\n", randomNumber);

    return 0;
}
```

La salida de muestra podría variar cada vez que ejecutes este programa:

```
Número Aleatorio: 42
```
Para generar números aleatorios dentro de un rango diferente, puedes ajustar el operador de módulo (`%`) de acuerdo con ello. Por ejemplo, `rand() % 10` genera números del 0 al 9.

Es importante notar que sembrar el generador de números pseudoaleatorios (`srand()`) con la hora actual (`time(NULL)`) asegura diferentes secuencias de números aleatorios en las ejecuciones del programa. Sin sembrar (`srand()`), `rand()` produciría la misma secuencia de números cada vez que se ejecuta el programa.

## Profundización

La función `rand()` y su contraparte de sembrado `srand()` han sido parte de la biblioteca estándar de C durante décadas. Se basan en algoritmos que generan secuencias de números que solo aparentan ser aleatorios, de ahí el término "pseudoaleatorios". El algoritmo subyacente en `rand()` es típicamente un generador congruencial lineal (LCG).

Aunque `rand()` y `srand()` son suficientes para muchas aplicaciones, tienen limitaciones conocidas, especialmente en lo que respecta a la calidad de la aleatoriedad y la potencial previsibilidad. Para aplicaciones que requieren aleatoriedad de alta calidad, como operaciones criptográficas, se deberían considerar alternativas como `/dev/random` o `/dev/urandom` (en sistemas similares a Unix), o API provistas por bibliotecas criptográficas.

Con la introducción de C11, el estándar ISO C incluyó un nuevo encabezado, `<stdatomic.h>`, ofreciendo un control más refinado para operaciones concurrentes, pero no directamente relacionado con la aleatoriedad. Para la verdadera aleatoriedad en C, los desarrolladores a menudo recurren a bibliotecas específicas de la plataforma o externas que ofrecen mejores algoritmos o hacen uso de fuentes de entropía de hardware.

Recuerda, mientras que `rand()` sirve como un medio simple y accesible para generar números pseudoaleatorios, sus usos en aplicaciones modernas están limitados por la calidad y previsibilidad de su salida. Cuando se requieren soluciones más robustas, especialmente para aplicaciones conscientes de la seguridad, explorar más allá de la biblioteca estándar es altamente recomendable.
