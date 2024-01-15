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

## ¿Por qué encontrar la longitud de una cadena de caracteres?

En programación, muchas veces necesitamos saber cuántos caracteres tiene una determinada cadena. Esto puede ser útil para hacer validaciones, crear bucles o simplemente para mostrar información al usuario con precisión.

## Cómo hacerlo

Para esto, utilizaremos la función `strlen()` de la biblioteca estándar de C. Esta función toma como parámetro una cadena de caracteres y devuelve un entero representando la longitud de dicha cadena.

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "¡Hola mundo!";
    int longitud = strlen(str);
    printf("La cadena \"%s\" tiene una longitud de %d caracteres.", str, longitud);
    return 0;
}
```

**Output:**

```
La cadena "¡Hola mundo!" tiene una longitud de 11 caracteres.
```

## Profundizando en la longitud de una cadena

Es importante destacar que la función `strlen()` realmente cuenta los caracteres hasta el primer carácter nulo `'\0'` que encuentre en la cadena. Por lo tanto, si nuestra cadena no incluye explícitamente un carácter nulo, obtendremos resultados inesperados.

Además, esta función solo cuenta los caracteres y no tiene en cuenta la codificación de caracteres, por lo que puede haber diferencias en la longitud dependiendo del lenguaje utilizado. También hay que tener en cuenta que los caracteres especiales o acentos pueden ser contados como más de un byte, lo que podría afectar el resultado final.

## Ver también

- [Documentación oficial de la función `strlen()` en C](https://en.cppreference.com/w/c/string/byte/strlen)
- [Guía de programación en C: Manejo de cadenas de caracteres](https://es.coursera.org/lecture/programacion-c/manejo-de-cadenas-de-caracteres-AiM8I) (Curso en línea)