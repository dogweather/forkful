---
title:    "C: Capitalizando una cadena"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena?

La capitalización de una cadena es un proceso común en la programación que implica convertir todas las letras de una cadena en mayúsculas o minúsculas. Esto puede ser útil para fines de presentación o para realizar comparaciones de cadenas. En esta publicación, aprenderemos cómo llevar a cabo la capitalización de cadenas en el lenguaje de programación C.

## Cómo hacerlo

Para capitalizar una cadena en C, necesitamos utilizar la función `toupper()` o `tolower()` de la biblioteca estándar `ctype.h`. La función `toupper()` convierte una letra en mayúscula, mientras que `tolower()` la convierte en minúscula. Ambas funciones aceptan un solo argumento de tipo `int`, que representa el carácter que deseamos convertir. A continuación, se muestra un ejemplo de código que capitaliza una cadena ingresada por el usuario:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main(){
    char str[100];
    printf("Ingresa una cadena: ");
    gets(str); // Se leen los datos del usuario
    for(int i = 0; i < strlen(str); i++){
        str[i] = toupper(str[i]); // Convertimos cada letra a mayúscula
    }
    printf("La cadena capitalizada es: %s", str); // Mostramos la cadena resultante
    return 0;
}
```
**Salida:**
```
Ingresa una cadena: Hola Mundo
La cadena capitalizada es: HOLA MUNDO
```

## Profundizando

La función `toupper()` y `tolower()` trabajan con la clasificación de caracteres `ASCII`, por lo que solo afectarán a los caracteres que se encuentren en ese rango. Esto significa que los caracteres acentuados no serán capitalizados. Además, si queremos capitalizar solo la primera letra de una palabra, podemos usar la función `toupper()` en conjunto con la función `tolower()`, que convertirá todas las letras a minúsculas excepto la primera.

Otra forma de capitalizar una cadena es utilizando la función `strlwr()` y `strupr()` de la biblioteca `string.h` que convierten una cadena completa en minúsculas o mayúsculas respectivamente. Estas funciones también trabajan con la clasificación de caracteres `ASCII`.

## Ver también

- [Tutorial de programación en C](https://programacion.net/c/tutorial)
- [Documentación de la biblioteca ctype.h](https://www.gnu.org/software/libc/manual/html_node/Character-Handling.html)
- [Documentación de la biblioteca string.h](https://www.tutorialspoint.com/c_standard_library/string_h.htm)