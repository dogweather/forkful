---
title:                "Comprobando si existe un directorio"
html_title:           "Go: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué comprobar si un directorio existe?

Para los programadores, verificar si un directorio existe es una tarea común al trabajar con archivos y procesos. Esto es especialmente importante si se están desarrollando aplicaciones que necesiten acceder y manipular archivos en una ubicación específica.

## Cómo hacerlo

La forma más sencilla de comprobar si un directorio existe en Go es utilizar la función `os.Stat()` y verificar si se produce un error. Si no hay error, significa que el directorio existe. A continuación se muestra un ejemplo de cómo se puede implementar esto en código:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Directorio que se desea verificar
    directorio := "mi/directorio"

    // Se utiliza la función os.Stat() para obtener información del directorio
    _, err := os.Stat(directorio)

    // Si se produce un error, significa que el directorio no existe
    if os.IsNotExist(err) {
        fmt.Printf("El directorio \"%s\" no existe.", directorio)
    } else {
        fmt.Printf("El directorio \"%s\" sí existe.", directorio)
    }
}
```

Este código imprimirá un mensaje indicando si el directorio existe o no. Si se desea hacer algo más con el directorio existente, se pueden usar las funciones `os.Mkdir()` o `os.MkdirAll()` para crearlo.

## Profundizando

Ahora que sabemos cómo verificar si un directorio existe, es importante entender cómo y dónde se aplican estas comprobaciones. En Go, es una buena práctica hacer estas verificaciones antes de manipular archivos o realizar otras operaciones en un directorio. También es importante tener en cuenta que la función `os.Stat()` también se utiliza para comprobar la existencia de un archivo, por lo que se puede utilizar de la misma manera para verificar tanto archivos como directorios.

## Ver también

- [Documentación de os.Stat() en la página oficial de Go](https://golang.org/pkg/os/#Stat)
- [Ejemplo de uso de os.Stat() en el blog de Go by Example](https://gobyexample.com/stat)