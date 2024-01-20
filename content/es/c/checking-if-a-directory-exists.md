---
title:                "Verificando si un directorio existe"
html_title:           "C: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Verificación si un Directorio Existe en C

## ¿Qué y Por Qué?
En programación, queremos saber si un directorio existe antes de usarlo, para evitar errores o problemas inesperados. Esto es especialmente relevante al manipular archivos y rutas.

## ¿Cómo hacerlo?
En C, puedes verificar la existencia de un directorio mediante el uso de la función `stat()` en la biblioteca `sys/stat.h`. Aquí se muestra un ejemplo de cómo hacerlo.

```C
#include <sys/stat.h>

int directory_exists(const char* path) {
   struct stat buffer;   
   return (stat(path, &buffer) == 0);
}

int main() { 
   if(directory_exists("/ruta/al/directorio")) {
       printf("El directorio existe.\n"); 
   } else {
       printf("El directorio no existe.\n");
   }
   return 0;
}
```

Si el directorio existe, este programa emitirá "El directorio existe." de lo contrario, emitirá "El directorio no existe.".

## Profundización

La función `stat()` ha existido desde los inicios de Unix, y sigue siendo una forma relevante de interactuar con los atributos de archivos y directorios. Sin embargo, no es la única manera de verificar la existencia de un directorio. Alternativamente podría usar `opendir()` de `dirent.h`, que intenta abrir el directorio, devolviendo NULL si falla.

Las implementaciones pueden variar según el sistema operativo y el sistema de archivos. Por ejemplo, algunas implementaciones de `stat()` fallarán en rutas que contienen enlaces simbólicos.

```C
#include <sys/stat.h>
#include <dirent.h>

int directory_exists(const char* path) {
   DIR* dir = opendir(path);
   if(dir) {
       closedir(dir);
       return 1;
   } else {
       return 0;
   }
}
```

Este código hace lo mismo que el anterior, pero usando `opendir()`. 

## Ver También

Para aprender más sobre la manipulación de directorios y archivos en C, aquí hay algunas fuentes útiles:

- [Documentación de `sys/stat.h`](http://pubs.opengroup.org/onlinepubs/7908799/xsh/sysstat.h.html)
- [Documentación de `dirent.h`](http://pubs.opengroup.org/onlinepubs/7908799/xsh/dirent.h.html)