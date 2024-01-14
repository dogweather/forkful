---
title:                "C++: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si estás interesado en aprender a programar en C++, una de las habilidades más importantes que debes dominar es la capacidad de leer argumentos de línea de comandos. Esto te permitirá crear programas más interactivos y dinámicos, y también te ayudará a desarrollar tus habilidades de resolución de problemas. Sigue leyendo para aprender cómo hacerlo.

## Cómo

Una de las formas más comunes de leer argumentos de línea de comandos en C++ es a través de la función "getopt". Esta función te permite especificar los argumentos que deseas leer y luego los almacena en variables para que puedas usarlos en tu programa. Aquí hay un ejemplo de cómo usarlo:

```C++
#include <unistd.h> 

int main(int argc, char *argv[]) { 
    // El primer argumento es el comando  
    // Los siguientes son los argumentos que estás buscando 
    // "a" y "b" son indicadores opcionales 
    int aflag = 0; 
    int bflag = 0; 
    char *cvalue = NULL; 
    int index; 
    int c; 

    opterr = 0; 

    while ((c = getopt(argc, argv, "abc:")) != -1) {
        switch (c) { 
            case 'a': 
                aflag = 1; 
                break; 
            case 'b': 
                bflag = 1; 
                break; 
            case 'c': 
                cvalue = optarg; 
                break; 
            case '?': 
                if (optopt == 'c') { 
                    fprintf(stderr, "La opción -%c requiere un argumento.\n", optopt); 
                } else if (isprint(optopt)) { 
                    fprintf(stderr, "Opción desconocida `-%c'.\n", optopt); 
                } else { 
                    fprintf(stderr, "Carácter de opción desconocido `\\x%x'.\n", optopt); 
                } 
                return 1; 
            default: 
                abort(); 
        } 
    } 

    printf("aflag = %d, bflag = %d, cvalue = %s\n", 
            aflag, bflag, cvalue); 

    for (index = optind; index < argc; index++) {
        printf("El argumento no válido que se encontró en %d es %s\n", index, argv[index]); 
    } 

    return 0; 
}
```

El siguiente es un ejemplo de cómo ejecutar este programa en la línea de comandos y su posible salida:

```bash
$ ./programa -a -c abc archivo1 archivo2
aflag = 1, bflag = 0, cvalue = abc
El argumento no válido encontrado en 4 es archivo1
El argumento no válido encontrado en 5 es archivo2
```

## Profundizando

Ahora que ya conoces la sintaxis básica de la función "getopt" para leer argumentos de línea de comandos en C++, puedes explorar más opciones como la función "getopt_long", que te permite especificar argumentos de una manera más flexible. También puedes experimentar con otras funciones para trabajar con argumentos de línea de comandos, como "argc" y "argv".

## Mira también

- Tutorial sobre lectura de argumentos de línea de comandos en C++: https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/
- Documentación de la función "getopt": http://www.cplusplus.com/reference/cstdlib/getopt/
- Tutorial sobre lectura de argumentos de línea de comandos con getopt_long: https://www.gnu.org/software/libc/manual/html_node/Getopt-Long-Option-Example.html