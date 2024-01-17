---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "C++: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Leer argumentos de línea de comando es una técnica común en la programación que permite a los desarrolladores pasar información a sus programas directamente desde la línea de comandos. Esto es útil para personalizar la ejecución de un programa y realizar tareas específicas sin tener que modificar el código.

## Cómo:

Para leer los argumentos de línea de comando en C++, se pueden seguir estos pasos:

1. Incluir la biblioteca `iostream` para poder utilizar las funciones de lectura.
2. Declarar una función `main` que acepte dos parámetros: `argc` (que indica el número de argumentos) y `argv` (que contiene los argumentos en forma de una matriz de cadenas).
3. Utilizar un bucle `for` para iterar a través de la matriz `argv` e imprimir el valor de cada argumento.

```
#include <iostream>

int main(int argc, char *argv[]) {
  for (int i = 0; i < argc; i++) {
    std::cout << argv[i] << std::endl;
  }
}
```

Ejemplo de salida para el comando `./programa argumento1 argumento2`:

```
./programa
argumento1
argumento2
```

## Profundizando

La lectura de argumentos de línea de comando se ha utilizado desde los primeros días de la programación. Antes de su existencia, los usuarios tenían que ingresar la información directamente en el programa, lo que limitaba su flexibilidad. Hoy en día, existen alternativas más avanzadas como la configuración de archivos o la interacción con interfaces gráficas, pero la lectura de argumentos de línea de comando sigue siendo una opción popular debido a su simplicidad y eficiencia.

Para acceder a argumentos más específicos, se pueden utilizar funciones de biblioteca como `getopt` o bibliotecas externas como `Boost.Program_options`. Es importante tener en cuenta que los argumentos de línea de comando están limitados en tamaño y no se pueden utilizar para pasar información sensible o confidencial.

## Ver también

- [Documentación oficial de C++ sobre argumentos de línea de comando](https://en.cppreference.com/w/cpp/language/main_function)
- [Ejemplos de uso de getopt en C++](https://www.geeksforgeeks.org/getopt-function-in-c-to-parse-command-line-arguments/)
- [Boost.Program_options](https://www.boost.org/doc/libs/1_72_0/doc/html/program_options/tutorial.html)