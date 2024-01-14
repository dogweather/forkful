---
title:                "C: Comprobando si existe un directorio"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##¿Por qué deberías comprobar si existe un directorio?

Comprobar si un directorio existe puede ser una tarea importante en la programación de C, especialmente en aplicaciones que requieren la manipulación de múltiples archivos y directorios. Al asegurarte de que un directorio existe antes de realizar cualquier operación en él, puedes evitar errores y asegurar que tu código funcione correctamente.

## Cómo hacerlo

Para comprobar si un directorio existe en C, puedes utilizar la función `opendir()` de la biblioteca `dirent.h`. Esta función devuelve un puntero al directorio si existe o `NULL` si no existe. Aquí hay un ejemplo de código para comprobar si un directorio llamado "prueba" existe:

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    DIR* dir = opendir("prueba");
    if (dir) {
        // Directorio existe, realizar operaciones aquí
        printf("El directorio existe");
        closedir(dir);
    }
    else {
        // Directorio no existe
        printf("El directorio no existe");
    }
    return 0;
}
```

El código anterior primero intenta abrir el directorio "prueba" utilizando `opendir()` y luego verifica si el valor de retorno es `NULL` o no. Si el directorio existe, se realizan las operaciones deseadas y se cierra el directorio utilizando `closedir()`. De lo contrario, se imprime un mensaje de error.

## Profundizando

Si deseas obtener más información sobre la comprobación de la existencia de un directorio en C, hay otros métodos que puedes utilizar. Por ejemplo, también puedes utilizar la función `access()` de la biblioteca `unistd.h` para verificar si un directorio existe o no.

```C
#include <stdio.h>
#include <unistd.h>

int main() {
    if (access("prueba", F_OK) != -1) {
        // Directorio existe
        printf("El directorio existe");
    }
    else {
        // Directorio no existe
        printf("El directorio no existe");
    }
    return 0;
}
```

Esta función devuelve 0 si el archivo o directorio especificado existe, en este caso "prueba", o -1 si no existe. Puedes usar esta función para verificar la existencia de cualquier tipo de archivo, no solo directorios.

## Ver también

- [Documentación de la función opendir() en C](https://www.gnu.org/software/libc/manual/html_node/Opening-a-Directory.html#Opening-a-Directory)
- [Documentación de la función access() en C](https://www.gnu.org/software/libc/manual/html_node/File-Permission-Tests.html#File-Permission-Tests)
- [Cómo trabajar con directorios en C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)