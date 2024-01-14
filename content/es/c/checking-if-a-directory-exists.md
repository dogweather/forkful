---
title:                "C: Verificando si existe un directorio"
simple_title:         "Verificando si existe un directorio"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

En el mundo de la programación, hay muchas veces que se necesita determinar si un directorio específico existe. Ya sea para asegurarse de que un archivo pueda ser leído o para evitar errores en el código, es importante saber cómo verificar la existencia de un directorio.

## Cómo hacerlo

Hay varias formas de verificar si un directorio existe en un programa escrito en C. Una forma es utilizar la función "opendir()" que abrirá un directorio para lectura. Si la función devuelve un puntero nulo, significa que el directorio no existe. Un ejemplo de código sería el siguiente:

```
#include <stdio.h> 
#include <string.h> 
#include <stdlib.h> 
#include <sys/types.h> 
#include <dirent.h> 

int main() 
{ 
    // Ruta del directorio a verificar 
    char* ruta = "/directorio/a/verificar"; 
    
    // Abrir el directorio 
    DIR* directorio = opendir(ruta); 
    
    // Comprobar si se pudo abrir el directorio 
    if(directorio == NULL) 
    { 
        printf("El directorio no existe.\n"); 
        return 1; 
    } 
    else
    { 
        printf("El directorio existe.\n"); 
        closedir(directorio); 
    } 
    
    return 0; 
} 
```

El código anterior imprimirá "El directorio existe" si se encuentra el directorio en la ruta especificada. Si no se encuentra, imprimirá "El directorio no existe".

## Deep Dive

Otra forma de verificar si un directorio existe es utilizando la función "stat()" que devuelve información sobre un archivo o directorio dado. Si se proporciona un directorio como argumento, la función devolverá un "0" como resultado si existe y "-1" si no existe. Un ejemplo de código sería el siguiente:

```
#include <stdio.h> 
#include <string.h> 
#include <sys/stat.h> 

int main() 
{ 
    // Ruta del directorio a verificar 
    char* ruta = "/directorio/a/verificar"; 
    
    // Declarar una variable de tipo structures stat 
    struct stat info; 
    
    // Utilizar la función stat() para verificar si el directorio existe 
    int resultado = stat(ruta, &info); 
    
    // Comprobar si la función devolvió "0" o "-1" 
    if(resultado == 0) 
    { 
        printf("El directorio existe.\n"); 
    } 
    else
    { 
        printf("El directorio no existe.\n"); 
    } 
    
    return 0; 
} 
```

Este código también imprimirá un mensaje dependiendo del resultado de la función "stat()". "El directorio existe" se imprimirá si se encuentra el directorio en la ruta especificada y "El directorio no existe" si no se encuentra.

## Ver también

- [Cómo crear y eliminar directorios en C](https://www.programiz.com/c-programming/c-file-input-output)
- [Funciones de gestión de directorios en C](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [Documentación oficial de la función "stat()"](http://man7.org/linux/man-pages/man2/stat.2.html)