---
title:    "Go: Escribiendo un archivo de texto"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por qué escribir archivos de texto en Go

Escribir archivos de texto es una tarea común en la programación, ya sea para guardar datos o para crear un registro de actividades de un programa. En Go, esta tarea puede ser realizada de una manera sencilla y eficiente, lo que la convierte en una habilidad valiosa para cualquier desarrollador.

## Cómo hacerlo
A continuación, se mostrará un ejemplo de cómo escribir un archivo de texto utilizando Go y el paquete "os":

```
package main

import (
  "fmt"
  "os"
)

func main() {
  // Crear o sobrescribir un archivo de texto llamado "datos.txt"
  archivo, err := os.Create("datos.txt")

  // Verificar si hubo algún error al crear el archivo
  if err != nil {
    fmt.Println("Hubo un error al crear el archivo:", err)
    return
  }

  // Escribir en el archivo
  archivo.WriteString("Esto es un archivo de texto\n")
  archivo.WriteString("Escrito con Go\n")

  // Cerrar el archivo
  archivo.Close()

  fmt.Println("Archivo de texto creado exitosamente.")
}
```

Al ejecutar este programa, se creará un archivo de texto llamado "datos.txt" con el siguiente contenido:

```
Esto es un archivo de texto 
Escrito con Go
```

## Profundizando en la escritura de archivos de texto
La función `os.Create()` utilizada en el ejemplo anterior también puede ser utilizada para abrir un archivo existente y agregarle más contenido. Además, el paquete "io/ioutil" ofrece funciones más avanzadas para escribir en archivos de texto, como por ejemplo la función `WriteFile()`, que permite escribir en un archivo en una sola línea de código.

También es importante tener en cuenta que al escribir en un archivo de texto en Go, se debe especificar el tipo de datos que se está escribiendo, ya que el paquete "os" maneja los datos como bytes. Esto se puede lograr utilizando la función `strconv.Itoa()` para convertir un número a cadena de caracteres, o `fmt.Sprintf()` para formatear una cadena.

## Ver también
- [Paquete "os" en la documentación de Go](https://golang.org/pkg/os/)
- [Paquete "io/ioutil" en la documentación de Go](https://golang.org/pkg/io/ioutil/)
- [Tutorial: Cómo escribir y leer archivos en Go](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-go)