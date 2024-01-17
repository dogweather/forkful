---
title:                "Encontrando la longitud de una cadena"
html_title:           "C: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En la programación, a menudo es necesario conocer la longitud o tamaño de una cadena de texto. Esto se refiere a la cantidad de caracteres que contiene la cadena. Los programadores necesitan conocer la longitud de una cadena por diversas razones, como por ejemplo, para validar entradas de usuario o para manipular y procesar la cadena de manera adecuada.

## ¿Cómo hacerlo?

En C, podemos encontrar la longitud de una cadena utilizando la función `strlen()`, que se encuentra en la biblioteca estándar `<string.h>`. Esta función toma como argumento una cadena de caracteres y devuelve su longitud.

```C
#include <stdio.h>
#include <string.h>
 
int main()
{
    char str[] = "Hola mundo";
    int longitud = strlen(str);
    printf("La longitud de la cadena es %d\n", longitud);

    return 0;
}
```

Este código imprimirá: `La longitud de la cadena es 10`, ya que la cadena "Hola mundo" tiene 10 caracteres.

## Profundizando

La función `strlen()` fue introducida en el estándar de C en la versión ANSI en 1989. Sin embargo, ya existía en sistemas UNIX desde hace tiempo. Existen otras formas de encontrar la longitud de una cadena, como iterar sobre la cadena contando los caracteres, pero la función `strlen()` es la forma más eficiente y recomendada en C.

En otros lenguajes de programación, como Python, la función para obtener la longitud de una cadena es simplemente `len()`, lo que demuestra su importancia en cualquier lenguaje de programación.

## Ver también

- [Referencia de la función strlen en C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Uso de la función strlen en C](https://www.geeksforgeeks.org/finding-length-of-string-in-c-programming/)