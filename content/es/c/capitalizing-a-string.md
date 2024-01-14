---
title:    "C: Capitalizando una cadena"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo nos encontramos con la necesidad de capitalizar una cadena de texto. Ya sea para mostrar una cadena en mayúsculas en la interfaz de usuario o para realizar ciertas operaciones en una cadena, el proceso de capitalizar es una tarea común en el desarrollo de software. En esta publicación de blog, exploraremos cómo capitalizar una cadena en el lenguaje de programación C y profundizaremos en el porqué de esta técnica.

## Cómo hacerlo

Para capitalizar una cadena en C, podemos seguir los siguientes pasos:

1. Primero, debemos declarar una variable de tipo `char` que almacene la cadena original.
2. Luego, creamos una segunda variable de tipo `char` donde almacenaremos la cadena capitalizada.
3. Utilizamos la función `strlen()` para obtener la longitud de la cadena original.
4. Creamos un bucle `for` que recorra cada letra de la cadena original.
5. Dentro del bucle, usamos la función `toupper()` para convertir cada carácter de la cadena original en mayúscula.
6. Guardamos el carácter convertido en la segunda variable que creamos.
7. Una vez finalizado el bucle, imprimimos la cadena capitalizada en pantalla.

A continuación, se presenta un ejemplo de código que implementa este proceso:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
    // Declaramos las variables
    char cadena_original[] = "¡hola mundo!";
    char cadena_capitalizada[100];
    int longitud = strlen(cadena_original);

    // Creamos un bucle que recorra cada letra de la cadena original
    for (int i = 0; i < longitud; i++) {
        // Utilizamos la función toupper() para convertir cada carácter en mayúscula
        cadena_capitalizada[i] = toupper(cadena_original[i]);
    }

    // Imprimimos la cadena capitalizada
    printf("La cadena original es: %s\n", cadena_original);
    printf("La cadena capitalizada es: %s\n", cadena_capitalizada);

    return 0;
}
```

El resultado de este código sería el siguiente:

```
La cadena original es: ¡hola mundo!
La cadena capitalizada es: ¡HOLA MUNDO!
```

## Profundizando

Detrás de la técnica de capitalizar una cadena en C, se encuentra una función importante llamada `toupper()`. Esta función, definida en la biblioteca `ctype.h`, convierte un carácter en mayúscula. Pero, ¿qué sucede internamente en esta función?

La función `toupper()` toma un único argumento de tipo `int` y devuelve un valor `int`. Primero comprueba si el valor que se le pasa es una letra minúscula. Si es así, le resta 32 en su representación ASCII para convertirla en mayúscula. Si el valor pasado no es una letra minúscula, devuelve el mismo valor sin modificarlo. Finalmente, devuelve el resultado.

Conociendo lo que sucede en la función `toupper()`, podemos ver cómo se implementa en nuestro ejemplo para capitalizar cada carácter de la cadena original.

## Ver también

Si te interesa saber más sobre las bibliotecas y funciones utilizadas en este artículo, puedes revisar la documentación oficial de C en los siguientes enlaces:

- [Función toupper()](https://en.cppreference.com/w/c/string/byte/toupper)
- [Biblioteca ctype.h](https://en.cppreference.com/w/c/header/cctype)
- [Otras funciones de la biblioteca string.h y su uso en C](https://www.geeksforgeeks.org/string-h-in-c-with-examples/)