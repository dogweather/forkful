---
title:                "Capitalizando una cadena"
html_title:           "C: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado convertir una cadena de texto a mayúsculas en un programa en C? La capitalización de cadenas es una tarea común en la programación y puede ser útil para mostrar títulos, validar entradas de usuario o simplemente para fines estéticos.

## Cómo hacerlo

Para capitalizar una cadena en C, podemos utilizar la función `toupper()` incluida en la biblioteca de strings `string.h`. Esta función toma un carácter como parámetro y devuelve su equivalente en mayúsculas. Podemos utilizar esta función en un bucle para recorrer cada carácter de la cadena y reemplazarlo con su versión en mayúsculas.

```
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {

    // Definir una cadena de ejemplo
    char str[] = "¡Hola, mundo!";

    // Obtener la longitud de la cadena
    int length = strlen(str);

    // Recorrer la cadena
    for (int i = 0; i < length; i++) {
        // Reemplazar los caracteres con su versión en mayúsculas
        str[i] = toupper(str[i]);
    }

    // Imprimir la cadena capitalizada
    printf("%s", str);

    return 0;
}
```

**Output:** ¡HOLA, MUNDO!

## Profundizando

La función `toupper()` es una de las muchas funciones útiles para manipular cadenas en C. Otras funciones útiles incluyen `tolower()` y `strcpy()`. También existen diferentes técnicas para capitalizar cadenas, por ejemplo, utilizando punteros en lugar de un bucle for. Puedes explorar más sobre estas técnicas y funciones en la documentación de C.

## Ver también

- [Documentación de C en español](https://es.cppreference.com/w/c)
- [Manipulación de cadenas en C](https://www.programiz.com/c-programming/c-strings)
- [Cadenas en C - Video tutorial en español](https://www.youtube.com/watch?v=ShcjNc5FFaM)