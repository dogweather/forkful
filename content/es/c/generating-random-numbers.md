---
title:                "Generación de números aleatorios"
date:                  2024-01-27T20:32:48.751827-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generación de números aleatorios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios en C implica crear secuencias de números que carecen de cualquier patrón discernible, imitando el concepto de aleatoriedad. Los programadores aprovechan los números aleatorios para una miríada de propósitos, incluyendo la simulación de datos, aplicaciones criptográficas y el desarrollo de juegos, lo que lo convierte en un aspecto vital de la programación.

## Cómo hacerlo:

Para generar números aleatorios en C, típicamente se usa la función `rand()` encontrada en `stdlib.h`. Sin embargo, es crucial sembrar el generador de números aleatorios para asegurar variabilidad en los números generados a través de diferentes ejecuciones del programa. La función `srand()`, sembrada con un valor, a menudo la hora actual, facilita esto.

Aquí hay un ejemplo simple de generar un número aleatorio entre 0 y 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Sembrar el generador de números aleatorios
    srand((unsigned) time(NULL));

    // Generar un número aleatorio entre 0 y 99
    int randomNumber = rand() % 100;

    // Imprimir el número aleatorio
    printf("Número Aleatorio: %d\n", randomNumber);

    return 0;
}
```

Salida de muestra:

```
Número Aleatorio: 42
```

Es importante notar que cada ejecución de este programa producirá un nuevo número aleatorio, gracias a la siembra con la hora actual.

## Un Vistazo Profundo

La forma tradicional de generar números aleatorios en C, usando `rand()` y `srand()`, no es verdaderamente aleatoria. Es pseudoaleatoria. Esto está bien para muchas aplicaciones, pero se queda corto en situaciones que requieren altos grados de aleatoriedad, como en usos criptográficos serios. La secuencia generada por `rand()` está completamente determinada por la semilla proporcionada a `srand()`. Por lo tanto, si la semilla es conocida, la secuencia puede ser predicha, reduciendo la aleatoriedad.

Históricamente, la función `rand()` ha sido criticada por su baja calidad de aleatoriedad y rango limitado. Alternativas modernas incluyen usar APIs específicas del dispositivo o bibliotecas externas que mejor aproximen la verdadera aleatoriedad o, en sistemas similares a UNIX, leer de `/dev/random` o `/dev/urandom` para propósitos criptográficos.

Por ejemplo, usando `/dev/urandom` en C:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // Abrir /dev/urandom para lectura
    fp = fopen("/dev/urandom", "r");

    // Leer un número aleatorio
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // Imprimir el número aleatorio
    printf("Número Aleatorio: %u\n", randomNumber);

    // Cerrar el archivo
    fclose(fp);

    return 0;
}
```

Este método lee directamente del pool de entropía del sistema, ofreciendo una calidad de aleatoriedad más alta adecuada para aplicaciones más sensibles. Sin embargo, este enfoque puede tener problemas de portabilidad en diferentes plataformas, haciéndolo menos universal que usar `rand()`.

Independientemente del método, entender la naturaleza de la aleatoriedad y su implementación en C es crucial para desarrollar aplicaciones efectivas, seguras y atractivas.
