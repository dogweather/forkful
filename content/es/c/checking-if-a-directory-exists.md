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

## Qué & Por qué?

Comprobar si un directorio existe es una tarea común para los programadores de C. Esto significa verificar si un directorio específico está presente en una ubicación dada en el sistema de archivos. Los programadores hacen esto para asegurarse de que su código pueda acceder y manipular los archivos en ese directorio sin causar errores.

## Cómo hacerlo:

Para comprobar si un directorio existe en C, podemos utilizar la función `opendir()` de la biblioteca `dirent.h`. Esta función toma el nombre del directorio como argumento y devuelve un puntero al directorio si existe, o `NULL` si no existe. Podemos utilizar una expresión condicional para verificar si el puntero está apuntando a `NULL` o no. Aquí hay un ejemplo de código:

```C
DIR *dir = opendir("/ruta/de/la/carpeta/");

if (dir == NULL) {
    printf("El directorio no existe.\n");
} else {
    printf("El directorio existe.\n");
    closedir(dir);
}
```

Si el directorio existe, el resultado será "El directorio existe." de lo contrario será "El directorio no existe."

## Inmersión profunda:

Antes de la introducción de `dirent.h` en las versiones más recientes de C, era necesario utilizar funciones de bajo nivel como `opendir()` de la biblioteca de C estándar `unistd.h` para comprobar si un directorio existe. También existen otras formas de lograr el mismo resultado, como utilizar el comando `stat` en sistemas tipo Unix o la API `PathFileExists()` en Windows.

En términos de implementación, la función `opendir()` utiliza la llamada al sistema `open()` para acceder al directorio. Además, `dirent.h` también proporciona funciones como `readdir()` y `closedir()` para obtener información sobre los archivos dentro del directorio y cerrarlo después de su uso.

## Ver también:

- [Documentación de `dirent.h` en cppreference.com](https://en.cppreference.com/w/c/io/dirent.h)
- [Ejemplo de uso de `PathFileExists()` en MSDN](https://docs.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathfileexistsa)