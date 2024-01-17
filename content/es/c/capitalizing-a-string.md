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

## ¿Qué y por qué?

Capitalizar una cadena de texto es el proceso de convertir todas las letras de una palabra en su forma mayúscula. Los programadores a menudo lo hacen para asegurarse de que todas las palabras en una cadena sigan un formato consistente y para facilitar la comparación de cadenas en un programa.

## Cómo hacerlo:

```
#include <stdio.h>
#include <string.h>

int main() {
    char cadena[] = "hola mundo";
    printf("Cadena antes de capitalizar: %s \n", cadena);
    int i;
    for (i = 0; i < strlen(cadena); i++) {
        cadena[i] = toupper(cadena[i]);
    }
    printf("Cadena después de capitalizar: %s \n", cadena);
    return 0;
}
```

```
Cadena antes de capitalizar: hola mundo 
Cadena después de capitalizar: HOLA MUNDO 
```

## Profundizando:

Capitalizar cadenas de texto tiene una larga historia en la programación. Antes de los lenguajes de programación modernos, como C, el almacenamiento de datos en una computadora era limitado, por lo que los programadores utilizaban letras mayúsculas para ahorrar espacio. Hoy en día, hay alternativas a la capitalización, como el uso de subcadenas y funciones de comparación que ignoran mayúsculas y minúsculas.

## Ver también:

Para obtener más información sobre cómo trabajar con cadenas de texto en C, consulta la documentación oficial: https://www.gnu.org/software/libc/manual/html_node/String-Functions.html