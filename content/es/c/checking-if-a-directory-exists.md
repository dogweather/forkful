---
title:                "Comprobando si existe un directorio"
html_title:           "C: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué comprobar si un directorio existe?

Comprobar si un directorio existe es una tarea común en la programación en C. Puede ser útil cuando se desea crear un nuevo directorio, verificar si un directorio específico está disponible para guardar datos o simplemente asegurarse de que un directorio necesario para el funcionamiento del programa existe.

## Cómo hacerlo

En C, existen varias formas de verificar si un directorio existe. Una manera simple es utilizando la función `opendir()` de la biblioteca `<dirent.h>` y comprobando si devuelve un puntero diferente de `NULL`. Aquí hay un ejemplo de código:

```C 
#include <stdio.h> 
#include <dirent.h> 

int main() 
{ 
    // Abrir el directorio
    DIR *dir = opendir("/home/usuario/mi_directorio"); 
    
    // Comprobar si se pudo abrir el directorio
    if (dir == NULL) 
    { 
        // El directorio no existe
        printf("El directorio no existe"); 
    } 
    else 
    { 
        // El directorio existe
        printf("El directorio existe"); 
        
        // Cerrar el directorio abierto
        closedir(dir); 
    } 
    
    return 0;
} 
```

El resultado de este código puede variar dependiendo del sistema operativo utilizado. Algunas posibles salidas podrían ser:

- Si el directorio existe: `El directorio existe`
- Si el directorio no existe: `El directorio no existe`

## Profundizando

Una forma más detallada de comprobar si un directorio existe es utilizando la función `stat()` de la biblioteca `<sys/stat.h>`. Esta función puede proporcionar información sobre un archivo o directorio específico, incluyendo su existencia. Aquí hay un ejemplo de código que utiliza `stat()` para verificar la existencia de un directorio:

```C 
#include <stdio.h> 
#include <sys/stat.h> 

int main() 
{ 
    // Crear una estructura para almacenar información sobre el directorio
    struct stat info; 
    
    // Obtener la información del directorio especificado
    int result = stat("/home/usuario/mi_directorio", &info); 
    
    // Comprobar el resultado
    if (result == 0) 
    { 
        // El directorio existe
        printf("El directorio existe"); 
    } 
    else 
    { 
        // El directorio no existe
        printf("El directorio no existe"); 
    } 
    
    return 0;
} 
```

Algunas posibles salidas de este código podrían ser:

- Si el directorio existe: `El directorio existe`
- Si el directorio no existe: `El directorio no existe`

Es importante tener en cuenta que esta función también puede proporcionar información sobre otros aspectos del directorio, como su tamaño o los permisos de acceso. Para obtener más información sobre la función `stat()`, se pueden consultar las páginas de manual utilizando el comando `man stat` en un sistema Linux.

## Ver también

- Tutorial de programación en C: https://www.programiz.com/c-programming
- Referencia de la biblioteca C estándar: https://www.ibm.com/docs/es/zos/2.3.0?topic=functions-introduction