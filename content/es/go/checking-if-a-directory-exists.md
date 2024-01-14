---
title:                "Go: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Porqué

A menudo, al trabajar con archivos y directorios en programas Go, es importante verificar si un directorio existe antes de intentar acceder a él. Esto puede ahorrar tiempo y evitar errores en el código.

## Cómo hacerlo

Para verificar si un directorio existe en Go, podemos utilizar la función `os.Stat()` que devuelve un objeto `os.FileInfo` con información sobre el archivo o directorio especificado. Luego, podemos verificar si el objeto `os.FileInfo` es nulo o no para determinar si el directorio existe.

Veamos un ejemplo en código para aclarar las cosas:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Directorio existente
    dir := "mydir" 

    // Directorio inexistente
    dir2 := "nonexistingdir"

    // Verificar si el primer directorio existe
    if _, err := os.Stat(dir); os.IsNotExist(err) {
        fmt.Println("El directorio", dir, "no existe")
    } else {
        fmt.Println("El directorio", dir, "existe")
    }

    // Verificar si el segundo directorio existe
    if _, err := os.Stat(dir2); os.IsNotExist(err) {
        fmt.Println("El directorio", dir2, "no existe")
    } else {
        fmt.Println("El directorio", dir2, "existe")
    }
}
```

En este ejemplo, utilizamos la función `os.Stat()` para obtener información sobre dos directorios diferentes, uno existente y otro inexistente. Si el directorio existe, no se producirá ningún error y el objeto `os.FileInfo` contendrá información sobre él. Sin embargo, si el directorio no existe, se producirá un error y podemos utilizar la función `os.IsNotExist()` para verificar si el error fue causado por la inexistencia del archivo o directorio.

La salida de este programa sería:

```
El directorio mydir existe
El directorio nonexistingdir no existe
```

## Un poco más profundo

Además de la función `os.Stat()`, también podemos utilizar la función `os.Mkdir()` para crear un nuevo directorio en caso de que no exista. Esta función devuelve un error en caso de que no se pueda crear el directorio, por lo que podemos usarla en conjunto con `os.IsNotExist()` para verificar si el directorio fue creado correctamente.

```Go
// Crear directorio si no existe
if _, err := os.Stat(dir); os.IsNotExist(err) {
    err := os.Mkdir(dir, 0755)
    if err != nil {
        fmt.Println("Error al crear el directorio", dir)
    }
}
```

## Ver también

Para obtener más información sobre el paquete `os` y todas sus funciones relacionadas, puedes consultar la documentación oficial de Go: https://golang.org/pkg/os/

También puedes echar un vistazo a estos tutoriales sobre cómo trabajar con archivos y directorios en Go:

- https://zetcode.com/golang/readfile/
- https://flaviocopes.com/go-list-files/
- https://www.calhoun.io/reading-files-in-go/