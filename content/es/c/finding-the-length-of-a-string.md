---
title:                "C: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo necesitamos saber la longitud de una cadena para realizar diversas tareas. Ya sea para validar la entrada del usuario, manipular datos o simplemente por curiosidad, encontrar la longitud de una cadena es una habilidad esencial para cualquier programador en C.

## Cómo

En C, la longitud de una cadena se refiere al número de caracteres que contiene, incluyendo espacios y símbolos. Para encontrar la longitud de una cadena, podemos usar la función `strlen()` que se encuentra en la biblioteca estándar `string.h`.

```C
#include <stdio.h>
#include <string.h>

int main() {
    // definir una cadena
    char cadena[] = "¡Hola, mundo!";

    // encontrar la longitud de la cadena
    int longitud = strlen(cadena);

    // imprimir la longitud
    printf("La longitud de la cadena es: %d", longitud);

    return 0;
}
```

**Salida:**

```
La longitud de la cadena es: 13
```

En este ejemplo, primero definimos una cadena llamada "¡Hola, mundo!" y luego usamos la función `strlen()` para encontrar su longitud. Finalmente, imprimimos la longitud en la pantalla.

## Profundizando

La función `strlen()` cuenta el número de caracteres de una cadena hasta encontrar el caracter nulo (`\0`), que marca el final de la cadena. Esto significa que la longitud de una cadena vacía (sin ningún carácter) es 0, mientras que la longitud de una cadena con solo un caracter nulo es 1.

Además, es importante recordar que la longitud devuelta por `strlen()` es de tipo `size_t`, que suele ser equivalente a `unsigned int`. Por lo tanto, si queremos almacenar la longitud de una cadena en una variable, debemos declararla como `size_t` o `unsigned int`.

## Ver también

- [Función `strlen()` en la documentación de C](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [Ejemplos adicionales de encontrar la longitud de una cadena en C](https://www.geeksforgeeks.org/strlen-function-in-c/)
- [Cómo contar el número de palabras en una cadena en C](https://www.freecodecamp.org/news/c-find-length-of-string/#:~:text=We%20can%20use%20the%20strlen,function%20to%20find%20the%20length.&text=We%20can%20calculate%20the%20number,positions%20in%20the%20character%20array.)