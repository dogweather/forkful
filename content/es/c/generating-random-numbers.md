---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:48:42.979436-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Generar números aleatorios es el proceso de producir valores que no siguen un patrón predecible. Los programadores lo hacen para cosas como juegos, simulaciones y seguridad, donde lo impredecible es valioso.

## Cómo:
Para generar números aleatorios en C, usualmente comenzamos con `srand` para "sembrar" el generador de números aleatorios, y usamos `rand` para obtener los números.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    srand(time(NULL)); // Inicializa la semilla con el tiempo actual
    for(int i = 0; i < 5; i++) {
        int aleatorio = rand() % 50 + 1; // Genera un número aleatorio entre 1 y 50
        printf("Número aleatorio %d: %d\n", i + 1, aleatorio);
    }
    return 0;
}
```

Cuando corres el programa, produces una salida como:
```
Número aleatorio 1: 23
Número aleatorio 2: 8
Número aleatorio 3: 42
Número aleatorio 4: 35
Número aleatorio 5: 17
```

## Profundizando:
Antiguamente, C no tenía un estándar para la generación de números aleatorios y las funciones variaban entre sistemas. Ahora, aunque `rand` y `srand` son estándares, no son ideales para necesidades criptográficas porque son predecibles. 

Alternativas modernas incluyen `/dev/random` en sistemas Unix o APIs de criptografía segura, aunque esas salen del estándar C. En implementación, `rand` puede usar algoritmos como el generador lineal congruencial (GLC) para producir secuencias de números que parecen aleatorias.

Para quien busca aleatoriedad más predecible y con mejores propiedades estadísticas, tenemos bibliotecas como `<random>` en C++ o módulos en otros lenguajes de programación diseñados específicamente para estas tareas.

## Ver También:
- Documentación de C estándar sobre la generación de números aleatorios: https://en.cppreference.com/w/c/numeric/random
- Una discusión sobre la seguridad de `rand` y `srand`: https://security.stackexchange.com/questions/12429/why-is-rand-considered-bad
- Guía de NIST sobre generadores de números aleatorios para propósitos criptográficos: https://csrc.nist.gov/publications/detail/sp/800-90a/rev-1/final