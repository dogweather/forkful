---
title:                "Eliminando comillas de una cadena"
date:                  2024-01-26T03:37:40.514761-07:00
model:                 gpt-4-0125-preview
simple_title:         "Eliminando comillas de una cadena"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Eliminar comillas de una cadena significa quitar cualquier marca de comillas, ya sean simples ('') o dobles (""), que formen parte del contenido de la cadena. Los programadores hacen esto para sanear la entrada, preparar datos para su posterior procesamiento, o evitar errores de sintaxis al tratar con rutas de archivos y comandos en lenguajes que utilizan comillas para delimitar cadenas.

## Cómo hacerlo:

Aquí tienes una función en C que eliminará esas molestas comillas de tus cadenas:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "Él dijo, \"Hola, 'mundo'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Saneado: %s\n", str);
    return 0;
}
```

Salida de ejemplo:

```
Original: Él dijo, "Hola, 'mundo'!"
Saneado: Él dijo, Hola, mundo!
```

## Análisis Profundo

Eliminar comillas de una cadena ha sido una tarea desde el amanecer de la programación, donde la higiene de datos fue y sigue siendo clave para evitar errores (como ataques de inyección SQL) o asegurarse de que una cadena pueda pasarse de manera segura a sistemas que podrían confundir una comilla con un carácter de control.

Históricamente, diferentes lenguajes manejan esta tarea de manera diferente; algunos tienen funciones incorporadas (como `strip` en Python), mientras que otros, como C, requieren una implementación manual debido a su enfoque en dar a los desarrolladores control a nivel más bajo.

Las alternativas incluyen usar funciones de biblioteca como `strpbrk` para encontrar comillas o emplear expresiones regulares (con bibliotecas como PCRE) para patrones más complejos, aunque esto podría ser excesivo simplemente para eliminar comillas.

La implementación anterior simplemente escanea cada carácter en la cadena, copiando solo los caracteres que no son comillas a la ubicación del puntero de escritura. Esto es eficiente porque se hace en el lugar sin necesidad de memoria extra para la cadena resultante.

## Ver También

- [Funciones de la Biblioteca Estándar de C](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Expresiones Regulares Compatibles con Perl](https://www.pcre.org/)
- [Entendiendo los Punteros en C](https://www.learn-c.org/en/Pointers)
