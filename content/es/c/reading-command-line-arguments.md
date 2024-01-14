---
title:                "C: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

El uso de argumentos de línea de comandos en la programación en C es una práctica muy común y útil. Permite a los usuarios proporcionar información adicional al programa al momento de su ejecución, lo que facilita su uso y personalización. Además, el uso de argumentos de línea de comandos también permite a los programadores ahorrar tiempo y esfuerzo al no tener que recompilar constantemente el código para cambiar los valores de entrada del programa.

## Cómo hacerlo

Para leer los argumentos de línea de comandos en un programa en C, se utilizan los parámetros "argc" y "argv". "argc" (argument count) representa el número de argumentos que se pasan al programa, mientras que "argv" (argument vector) es una matriz que contiene todos los argumentos como cadenas de caracteres.

A continuación se muestra un ejemplo de código que utiliza estos parámetros para leer los argumentos de línea de comandos y mostrarlos en pantalla:

```C
#include <stdio.h>

int main(int argc, char *argv[])
{
    // El primer argumento (argv[0]) siempre es el nombre del programa
    printf("El nombre del programa es: %s\n", argv[0]);

    // Se recorren los demás argumentos con un bucle for
    for (int i = 1; i < argc; i++)
    {
        printf("Argumento %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Si se compila y ejecuta este programa con los siguientes argumentos de línea de comandos:

```
./programa argumento1 "argumento 2" 3
```

La salida sería la siguiente:

```
El nombre del programa es: ./programa
Argumento 1: argumento1
Argumento 2: argumento 2
Argumento 3: 3
```

## Profundizando

Además de los argumentos de línea de comandos estándar, también es posible utilizar argumentos opcionales utilizando la función "getopt()". Esta función permite especificar opciones, como en el comando "ls -l", donde "l" es una opción.

Además, también se pueden procesar argumentos de línea de comandos de forma más compleja utilizando bibliotecas externas, como "argp" o "popt", que proporcionan herramientas adicionales para trabajar con argumentos de línea de comandos.

## Ver también

- [Tutorial: Leer argumentos de línea de comandos en C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Ejemplos de uso de getopt() en C](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html)
- [Documentación de la función getopt() en C](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)