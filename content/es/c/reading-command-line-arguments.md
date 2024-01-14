---
title:                "C: Leyendo argumentos de línea de comando"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# ¿Por qué leer argumentos de línea de comandos en C?

Los argumentos de línea de comandos son una herramienta esencial en el mundo de la programación, especialmente en el lenguaje C. Permiten que los usuarios pasen información a un programa desde la línea de comandos, lo que lo hace más versátil y personalizable. En esta publicación, aprenderemos cómo leer y utilizar argumentos de línea de comandos en C.

## Cómo hacerlo

Para leer los argumentos de línea de comandos en C, utilizamos la función `main()` y sus parámetros `argc` y `argv`. `argc` (argument count) es una variable entera que contiene el número de argumentos pasados al programa, mientras que `argv` (argument vector) es una matriz de cadenas que contiene los argumentos en sí. Veamos un ejemplo:

```C
// Ejemplo de código para leer argumentos de línea de comandos
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("El número de argumentos pasados es %d\n", argc);
  for(int i = 0; i < argc; i++) {
    printf("Argumento %d: %s\n", i, argv[i]);
  }
  return 0;
}
```

Este código imprimirá el número de argumentos pasados al programa y cada uno de los argumentos en una línea separada. Ahora, si ejecutamos el programa con algunos argumentos como `./programa Hola mundo`, obtendremos la siguiente salida:

```
El número de argumentos pasados es 3
Argumento 0: ./programa
Argumento 1: Hola
Argumento 2: mundo
```

Podemos ver que el primer argumento (`argv[0]`) siempre es el nombre del programa. Los siguientes son los argumentos que hayamos pasado en la línea de comandos.

## Profundizando

Además de leer los argumentos de línea de comandos, también podemos validarlos y utilizarlos en nuestro programa. Por ejemplo, podríamos crear un programa que convierte una temperatura de grados Celsius a Fahrenheit utilizando un argumento de línea de comandos. Para ello, necesitamos asegurarnos de que el usuario ingrese un argumento numérico. Podemos hacerlo utilizando la función `atoi()` para convertir `argv[1]` en un número entero y luego verificar si el resultado es mayor que cero (ya que no tendría sentido convertir una temperatura menor a cero en grados Celsius). Aquí hay un ejemplo de código:

```C
#include <stdio.h>
#include <stdlib.h> // Para utilizar la función atoi()

int main(int argc, char *argv[]) {
  if(argc == 2) { // Verifica si se ha pasado un único argumento
    int celsius = atoi(argv[1]); // Convierte el argumento a un número entero
    if(celsius > 0) { // Verifica si es mayor a cero
      float fahrenheit = ((celsius * 9) / 5) + 32; // Convierte a Fahrenheit
      printf("%d grados Celsius equivale a %.2f grados Fahrenheit\n", celsius, fahrenheit);
    } else {
      printf("Ingrese un valor mayor a cero\n");
    }
  } else {
    printf("Ingrese solo un argumento: temperatura en grados Celsius\n");
  }
  return 0;
}
```

Al ejecutar este programa con `./programa 25`, obtendremos la siguiente salida:

```
25 grados Celsius equivale a 77.00 grados Fahrenheit
```

## Ver también

- [Documentación de argc y argv en C](https://www.programiz.com/c-programming/c-command-line-arguments)
- [Más sobre la función atoi() en C](https://www.geeksforgeeks.org/atoi-function-in-c-cpp/)