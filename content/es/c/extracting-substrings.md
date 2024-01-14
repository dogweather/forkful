---
title:                "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas?

Extraer subcadenas es una técnica útil en la programación en C que permite trabajar con partes específicas de una cadena de texto. Esto puede ser especialmente útil cuando se necesita manipular o analizar datos dentro de una cadena más grande.

## Cómo hacerlo

Para extraer una subcadena en C, primero debemos tener en cuenta dos puntos importantes: la ubicación inicial de la subcadena (índice de inicio) y su longitud (número de caracteres). Con esta información, podemos utilizar la función `strncpy` para copiar una cantidad específica de caracteres desde una cadena de origen a una cadena de destino.

Veamos un ejemplo:

```
#include <stdio.h>
#include <string.h>

int main() {
    char cadena[] = "Hola mundo";
    char subcadena[5];
    int inicio = 5;
    int longitud = 5;

    strncpy(subcadena, cadena + inicio, longitud);
    printf("Subcadena: %s\n", subcadena);

    return 0;
}
```

Este código tendrá una cadena original de "Hola mundo" y extraerá una subcadena de longitud 5 a partir del índice 5, que sería "mundo". Luego, se imprimirá la subcadena resultante, que debería ser "mundo".

## Profundizando en la extracción de subcadenas

Para entender mejor cómo funciona el proceso de extracción de subcadenas en C, es importante conocer cómo se almacenan las cadenas y cómo se accede a sus elementos.

En C, una cadena es básicamente un array de caracteres. Cada carácter se almacena en una posición del array y se pueden acceder individualmente utilizando su índice. Por ejemplo, en una cadena "hola", la "h" tendría un índice de 0, la "o" tendría un índice de 1 y así sucesivamente.

Con esto en mente, podemos ver cómo la función `strncpy` funciona para extracción de subcadenas. Utilizando el índice de inicio y la longitud, simplemente le decimos a la función cuáles son los elementos específicos que queremos copiar a nuestra cadena de destino.

## Ver También

- [Función strncpy en la documentación de C](https://www.codingame.com/playgrounds/25669/funcion-para-copiar-subcadenas-en-c-strncpy)
- [Manejo de cadenas en C](https://www.geeksforgeeks.org/c-program-demonstrating-string-manipulations/)

¡Esperamos que este artículo te haya sido útil para aprender sobre la extracción de subcadenas en C! Ahora puedes aplicar esta técnica en tus propios proyectos de programación. ¡Hasta la próxima!