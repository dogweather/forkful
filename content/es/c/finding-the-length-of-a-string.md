---
title:    "C: Encontrando la longitud de una cadena"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, una tarea común es la de encontrar la longitud de una cadena de caracteres. Esto puede ser útil para muchas cosas, como validar la entrada de datos del usuario o manipular cadenas de manera eficiente. En este artículo, aprenderemos cómo encontrar la longitud de una cadena en el lenguaje de programación C.

## Cómo hacerlo

Para encontrar la longitud de una cadena en C, utilizaremos la función "strlen". Esta función se encuentra en la biblioteca estándar de C, por lo que no es necesario importar ninguna biblioteca adicional.

```C
#include <stdio.h>
#include <string.h>

int main() {
    // Definimos una cadena de caracteres
    char str[] = "Hola Mundo!";
    // Llamamos a la función strlen y almacenamos el resultado en una variable
    int length = strlen(str);
    // Imprimimos la longitud de la cadena
    printf("La longitud de la cadena es: %d", length);

    return 0;
}
```

El resultado de este código sería: "La longitud de la cadena es: 11". Como se puede ver, la función strlen nos devuelve el número de caracteres en la cadena, incluyendo espacios y puntuación.

## Profundizando

Si te preguntas cómo funciona exactamente la función "strlen", aquí hay un poco más de información. La función recorre la cadena de caracteres hasta que encuentra el caracter nulo ('\0'), que indica el final de la cadena. Cada vez que recorre un caracter, aumenta el contador de longitud en 1. Al final, devuelve el valor del contador como resultado.

Una cosa importante a tener en cuenta es que la función "strlen" solo funciona con cadenas que terminan con el caracter nulo ('\0'). Si una cadena no tiene este caracter al final, la función podría seguir recorriendo la memoria y causar errores. Por lo tanto, siempre debemos asegurarnos de que nuestras cadenas estén correctamente formateadas.

## Ver también

- [Tutorial de strlen en C](https://www.programiz.com/c-programming/library-function/string.h/strlen)
- [Explicación detallada sobre el funcionamiento de strlen](https://www.geeksforgeeks.org/strlen-function-in-c/)
- [Otras funciones para trabajar con cadenas en C](https://en.cppreference.com/w/cpp/string/byte)