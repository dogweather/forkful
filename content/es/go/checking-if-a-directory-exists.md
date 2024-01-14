---
title:                "Go: Verificar si existe un directorio"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Algunas veces, al escribir un programa en Go, es necesario verificar si un directorio existe antes de realizar ciertas acciones. Esto puede ser útil para evitar errores o para tomar decisiones basadas en la existencia o falta de un directorio.

## Cómo hacerlo

Existen varias formas de verificar si un directorio existe en Go. Una forma es utilizar la función `os.Stat(path)`, que devuelve un objeto `FileInfo` si el directorio existe. Si el directorio no existe, se lanzará un error y podemos manejarlo adecuadamente en nuestro código. Veamos un ejemplo de cómo utilizar esta función:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Definimos el path del directorio a verificar
    path := "/tmp/example"

    // Verificamos si existe utilizando os.Stat()
    _, err := os.Stat(path)

    if err != nil {
        // Si el error no es del tipo "archivo o directorio no existe",
        // manejamos el error
        if os.IsNotExist(err) {
            fmt.Println("El directorio no existe")
        } else {
            fmt.Println("Error:", err)
        }
    } else {
        fmt.Println("El directorio existe")
    }
}
```

En este ejemplo, utilizamos la función `os.IsNotExist()` para verificar específicamente si el error es debido a que el directorio no existe. De esta forma, podemos tomar medidas diferentes en base al tipo de error que se genere.

Otra forma de verificar la existencia de un directorio es utilizando la función `os.MkdirAll(path, perm)`, que crea el directorio si no existe o no hace nada si el directorio ya existe. Una ventaja de esta función es que también se encarga de crear cualquier directorio padre necesario en la ruta especificada. Veamos un ejemplo de cómo utilizar esta función:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Definimos el path del directorio a crear
    path := "/tmp/example/new_directory"

    // Creamos el directorio utilizando os.MkdirAll()
    err := os.MkdirAll(path, 0755)

    if err != nil {
        fmt.Println("Error:", err)
    } else {
        fmt.Println("El directorio fue creado o ya existe")
    }
}
```

## Profundizando

En el apartado anterior, vimos dos formas diferentes de verificar la existencia de un directorio en Go. Sin embargo, es importante tener en cuenta que el manejo de errores es crucial. Si no se manejan adecuadamente, pueden ocurrir errores inesperados en nuestro código.

Por ejemplo, si intentamos crear un archivo en un directorio que no existe, no tendremos un error al momento de la creación del archivo ya que `os.MkdirAll()` no lanzará un error si el directorio ya existe o no puede ser creado. En este caso, deberíamos utilizar la función `os.Stat()` previamente para asegurarnos de que el directorio exista antes de crear el archivo.

En conclusión, al trabajar con directorios en Go es importante tener en cuenta las diferentes opciones para verificar su existencia y manejar adecuadamente los errores que puedan surgir.

## Ver también

- [Documentación oficial de os.Stat en el sitio web de Go](https://golang.org/pkg/os/#Stat)
- [Ejemplos de os.Stat en el libro "The Go Programming Language"](https://tour.golang.org/welcome/11)
- [Documentación oficial de os.MkdirAll en el sitio web de Go](https://golang.org/pkg/os/#MkdirAll)
- [Ejemplos de os.MkdirAll en el libro "The Go Programming Language"](https://tour.golang.org/welcome/12)