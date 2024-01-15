---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "C: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Si estás aprendiendo a programar en C, es importante entender cómo leer los argumentos de línea de comando. Esto te permitirá crear programas que puedan recibir información externa y tomar decisiones basadas en ella. Además, entender cómo funcionan los argumentos de línea de comando puede ayudarte a depurar tu código y hacerlo más eficiente.

## Cómo hacerlo

Para leer los argumentos de línea de comando en C, necesitas usar los parámetros de la función `main` y la variable `argc` (count) que contiene el número de argumentos ingresados. Con estos, puedes acceder a los argumentos individuales a través de la variable `argv` (argument vector) y su índice correspondiente. Aquí hay un ejemplo:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("Número de argumentos ingresados: %d\n", argc);
  for (int i = 0; i < argc; i++) {
    printf("Argumento %d: %s\n", i, argv[i]);
  }
  return 0;
}
```

Al ejecutar este programa con los siguientes argumentos en la línea de comando `./ejemplo hola mundo`, obtendrías la siguiente salida:

```
Número de argumentos ingresados: 3
Argumento 0: ./ejemplo
Argumento 1: hola
Argumento 2: mundo
```

Si necesitas convertir los argumentos a tipos de datos diferentes, puedes usar las funciones `atoi` o `atof` para convertirlos a enteros o números de punto flotante, respectivamente.

## Profundizando en el tema

Además de los argumentos regulares, también puedes acceder a la ubicación del programa ejecutable con `argv[0]` y a los argumentos ingresados como opciones con la librería de funciones `getopt`. También es importante tener en cuenta que los argumentos de línea de comando se pueden pasar en cualquier orden y pueden contener comillas para incluir espacios en blanco dentro de un argumento.

## Ver también

Si quieres aprender más sobre cómo trabajar con argumentos de línea de comando en C, asegúrate de revisar estos recursos:

- [Documentación oficial sobre argumentos de línea de comando en C](https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html)
- [Tutorial en español sobre argumentos de línea de comando en C](https://www.leasys.ro/news/mirrors/Various/Programming/moxman/C%20-%20Como%20leer%20argumentos%20de%20linea%20de%20comando.htm)
- [Ejemplos de código sobre argumentos de línea de comando en C](https://www.thegeekstuff.com/2013/01/c-argc-argv/)