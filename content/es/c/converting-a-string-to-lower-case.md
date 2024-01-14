---
title:                "C: Convirtiendo una cadena a minúsculas"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

En programación, es común encontrarse con la necesidad de convertir una cadena de caracteres a minúsculas. Esto es especialmente útil cuando se están comparando dos cadenas y se desea ignorar las diferencias en mayúsculas y minúsculas. Afortunadamente, en C existe una función incorporada que nos permite realizar esta conversión de manera sencilla y eficiente.

## Cómo hacerlo

Para convertir una cadena a minúsculas en C, utilizamos la función `tolower()` de la biblioteca `ctype.h`. Esta función toma como argumento un carácter y devuelve su equivalente en minúscula. Sin embargo, para poder utilizarla con una cadena completa, necesitamos recorrer cada carácter de la cadena y aplicarle la función `tolower()`.

```c
#include <stdio.h>
#include <ctype.h>

int main() {
    char cadena[] = "EN ESTA CADENA TODO ESTÁ EN MAYÚSCULAS";
    
    // Recorremos la cadena y aplicamos tolower()
    for (int i = 0; cadena[i] != '\0'; i++) {
        cadena[i] = tolower(cadena[i]);
    }
    
    printf("%s\n", cadena);
    // Output: en esta cadena todo está en mayúsculas
}
```

## Profundizando

La función `tolower()` utiliza la tabla de caracteres ASCII (American Standard Code for Information Interchange) para realizar la conversión. Esta tabla asigna un número entero a cada carácter, y las letras mayúsculas y minúsculas tienen sus propios códigos que se encuentran a una distancia constante de 32. Por ejemplo, la letra `A` tiene un código de 65, mientras que la letra `a` tiene un código de 97. Por lo tanto, para convertir una letra mayúscula a minúscula, solo tenemos que sumar 32 a su código.

Otra cosa importante a tener en cuenta es que esta función solo funciona para caracteres alfabéticos. Si intentamos convertir un carácter numérico u otro tipo de carácter, no veremos ningún cambio.

## Ver también

- [Función tolower() en la documentación de C](https://www.programiz.com/c-programming/library-function/ctype.h/tolower)
- [Tabla de caracteres ASCII](https://es.wikipedia.org/wiki/ASCII)