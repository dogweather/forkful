---
title:                "Lectura de argumentos de línea de comandos"
date:                  2024-01-20T17:55:39.904213-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de argumentos de línea de comandos"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Leer argumentos de la línea de comandos permite que tu programa en C reciba datos de entrada al ejecutarse, como configuraciones o archivos a procesar. Los programadores lo hacen para que sus programas sean más versátiles y puedan adaptarse a diferentes situaciones sin necesidad de cambiar el código.

## Cómo Hacerlo:
Vamos directo al grano. Los argumentos de línea de comandos en C se manejan a través de dos parámetros en la función `main`: `argc` para el número de argumentos, e `argv` para los valores de los argumentos.

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Nombre del programa: %s\n", argv[0]);
    for (int i = 1; i < argc; i++) {
        printf("Argumento %d: %s\n", i, argv[i]);
    }
    return 0;
}
```
Si compilas y ejecutas este código con `./mi_programa arg1 arg2`, tendrás como salida:
```
Nombre del programa: ./mi_programa
Argumento 1: arg1
Argumento 2: arg2
```

## Inmersión Profunda:
Históricamente, la habilidad de pasar argumentos a través de la línea de comandos es tan antigua como los propios sistemas Unix, permitiendo a los usuarios y a los scripts manejar programas de forma dinámica. Como alternativas, podrías considerar la lectura de variables de entorno, archivos de configuración, o incluso interacción interactiva con el usuario.

La implementación particular de esta característica en C implica que `argv` es un array de strings (`char*`), donde `argv[0]` es el nombre del programa y `argv[argc]` es un puntero `NULL`. Es esencial comprobar que `argc` es suficiente antes de acceder a `argv[i]` para evitar errores de segmentación.

## Ver También:
- La página `man` de exec(3) en Unix/Linux provee detalles sobre cómo los programas reciben argumentos:
  ```
  man 3 exec
  ```
- C Standard Library Reference: `stdlib.h` y `string.h` pueden proporcionar funciones útiles cuando trabajas con argumentos de línea de comandos.
- GNU `getopt` function para el manejo avanzado de argumentos de línea de comandos:
  https://www.gnu.org/software/libc/manual/html_node/Getopt.html