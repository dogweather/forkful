---
title:    "C: Comprobando si existe un directorio"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
Revisar si un directorio existe es una de las tareas básicas de programación que puede ayudar a los desarrolladores a verificar la estructura de un sistema de archivos y garantizar que sus programas funcionen correctamente.

## Cómo
Las siguientes son dos formas de verificar si un directorio existe en C:

```C
// Ejemplo 1: Utilizando la función opendir()
#include <stdio.h>
#include <dirent.h>

int main()
{
    // Definir la ruta del directorio a verificar
    char *path = "/home/usuario/carpeta";

    // Abrir el directorio y almacenar un puntero al mismo en la variable dir
    DIR *dir = opendir(path);

    // Verificar si la variable dir es igual a NULL, lo que indica que el directorio no existe
    if (dir == NULL)
    {
        // Imprimir mensaje en caso de que el directorio no exista
        printf("El directorio %s no existe.", path);
    }
    else
    {
        // Imprimir mensaje en caso de que el directorio exista
        printf("El directorio %s existe.", path);
    }

    // Cerrar el directorio
    closedir(dir);

    return 0;
}
```

```C
// Ejemplo 2: Utilizando la función stat()
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

int main()
{
    // Definir la ruta del directorio a verificar
    char *path = "/home/usuario/carpeta";

    // Crear una estructura stat donde se almacenará la información del directorio
    struct stat st;

    // Verificar si la función stat devuelve un valor diferente de 0, lo que indica que el directorio no existe
    if (stat(path, &st) != 0)
    {
        // Imprimir mensaje en caso de que el directorio no exista
        printf("El directorio %s no existe.", path);
    }
    else
    {
        // Imprimir mensaje en caso de que el directorio exista
        printf("El directorio %s existe.", path);
    }

    return 0;
}
```

Los ejemplos anteriores utilizan dos funciones diferentes para comprobar la existencia de un directorio. La función opendir() abre el directorio y devuelve un puntero a él, mientras que la función stat() devuelve información sobre el archivo o directorio especificado. Ambos tienen en cuenta que si el directorio no existe, se devolverá un valor nulo o un valor diferente de 0.

## Profundizando
Hay diferentes razones por las que un directorio puede no existir. Puede ser que el directorio haya sido eliminado o que la ruta especificada esté incorrecta. En el caso de la función opendir(), también hay que tener en cuenta que el directorio puede no existir debido a permisos insuficientes o a un problema en el sistema de archivos.

Por otro lado, también se pueden utilizar otras funciones como access() o opendirat() para verificar la existencia de un directorio en C. Cada función tiene sus propias ventajas y limitaciones, por lo que es importante elegir la más adecuada según el contexto de cada proyecto.

## Ver también
- [Función opendir() en C](https://www.man7.org/linux/man-pages/man3/opendir.3.html)
- [Función stat() en C](https://www.man7.org/linux/man-pages/man3/stat.3.html)
- [Función access() en C](https://www.man7.org/linux/man-pages/man2/access.2.html)
- [Función opendirat() en C](http://man7.org/linux/man-pages/man3/opendirat.3.html)