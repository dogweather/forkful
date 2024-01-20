---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Leer los argumentos de la línea de comando significa aceptar entradas cuando ejecutas el programa desde el terminal. Permiten a los programadores ejecutar el programa con diferentes configuraciones sin tener que cambiar y recompilar el código.

## Cómo se hace:

Aquí hay un ejemplo de cómo usar los argumentos de la línea de comando:

```C
#include <stdio.h>
int main(int argc, char *argv[]) 
{
   printf("Program name %s\n", argv[0]);

   if( argc == 2 ) 
   {
      printf("The argument supplied is %s\n", argv[1]);
   }
   else if( argc > 2 ) 
   {
      printf("Too many arguments supplied.\n");
   }
   else 
   {
      printf("One argument expected.\n");
   }
}
```

Si ejecutas este programa con el nombre `test` y un argumento adicional `hello`, obtendrás:

```
Program name test
The argument supplied is hello
```

## Inmersión Profunda

Los argumentos de la línea de comandos son una práctica antigua, existiendo desde los primeros días de Unix. Son muy útiles, pueden ser una herramienta poderosa en tus programas.

Las alternativas a los argumentos de la línea de comandos incluyen leer desde archivos de configuración o solicitar entrada durante la ejecución.

Cuando ejecutas un programa, el sistema operativo asigna memoria para el nombre del programa y los argumentos y los organiza en un array de cadenas. El argumento `argc` es la cuenta de argumentos de línea de comandos y `argv` es un puntero a este array.

## Ver También

- Documentación ISO/IEC 9899, sección 5.1.2.2.1: Detalles formales sobre `main` y argumentos de la línea de comando
- Libros de programación en C: Para una comprensión más profunda de los conceptos en C
- https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html#Program-Arguments: Para más detalles sobre los argumentos de línea de comando.