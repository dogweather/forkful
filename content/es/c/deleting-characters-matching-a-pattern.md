---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "C: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces, al trabajar con una gran cantidad de texto o datos en un programa escrito en C, nos podemos encontrar con la necesidad de eliminar ciertos caracteres que coinciden con cierto patrón. Esto puede ser por varios motivos, como limpiar el texto antes de procesarlo o simplemente por necesidad del programa. En este artículo, veremos cómo podemos lograr esto de una manera sencilla y eficiente.

## Cómo

Para eliminar caracteres que coinciden con un patrón, utilizaremos la función `strspn()` de la biblioteca `string.h` en C. Esta función nos permite obtener la longitud de una subcadena que cumple con cierto patrón en una cadena dada. A continuación, utilizaremos la función `memmove()` para mover los caracteres no deseados hacia la izquierda, eliminándolos.

Por ejemplo, supongamos que queremos eliminar todas las vocales de una palabra. Utilizaremos `strspn()` para obtener la longitud de la subcadena que contenga todas las vocales, luego usaremos esa longitud en la función `memmove()` para eliminarlas y finalmente imprimiremos la nueva palabra.

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char palabra[10] = "Hola";
    int longitud = strspn(palabra, "aeiou");
    memmove(palabra, palabra + longitud, strlen(palabra) - longitud + 1);
    // Imprime "Hl"
    printf("%s", palabra);
    return 0;
}
```

## Inmersión Profunda

La función `strspn()` busca en la cadena dada hasta encontrar un caracter que no coincida con el patrón dado. Devolverá la longitud de la subcadena encontrada, que será igual a la posición donde se encuentra dicho carácter. En el ejemplo anterior, se encontró una vocal en la posición 1, por lo que la subcadena empezaría en la posición 0 y la función devolvería 1.

La función `memmove()` se utiliza para mover bloques de memoria de manera eficiente. En este caso, utilizamos la dirección de memoria después de la subcadena encontrada como origen y la dirección de memoria antes de dicha subcadena como destino. Luego, especificamos la longitud de los caracteres que queremos mover, es decir, la longitud de la cadena original menos la longitud de la subcadena encontrada más 1 para incluir el caracter nulo. Esto resultará en que los caracteres que no queremos sean sobrescritos por los caracteres que sí queremos mantener.

## Ver también

- Documentación oficial de `string.h`: https://www.cplusplus.com/reference/cstring/
- Tutorial interactivo sobre cadenas en C: https://www.learn-c.org/en/Strings
- Ejemplos de código para operaciones de cadenas en C: https://www.geeksforgeeks.org/c-programming-language