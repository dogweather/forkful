---
title:                "Escritura de un archivo de texto"
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir un archivo de texto en Go es guardar datos en un fichero que puedes leer luego. Los programadores lo hacen para guardar configuraciones, resultados o compartir información entre programas y sesiones.

## Cómo Hacerlo:

Para escribir en un archivo en Go, usas el paquete `os`. Aquí tienes un ejemplo simple:

```Go
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    archivo, err := os.Create("ejemplo.txt")
    if err != nil {
        panic(err)
    }
    defer archivo.Close()

    escritor := bufio.NewWriter(archivo)
    _, err = escritor.WriteString("Hola, ¿qué tal?\n")
    if err != nil {
        panic(err)
    }

    escritor.Flush()
}

```

Este código crea un archivo `ejemplo.txt` y escribe la línea "Hola, ¿qué tal?". Si el archivo ya existe, será sobreescrito.

## Profundizando

Históricamente, escribir en archivos ha sido una necesidad para persistir datos. Antes se usaban cintas y hoy discos o SSDs. En Go, además del `bufio` y `os`, puedes usar `ioutil`, aunque desde Go 1.16 está en desuso y se prefieren `os` y `io`. La escritura puede ser síncrona (esperando a que se complete) o asíncrona (en background), afectando el rendimiento y la respuesta.

## Ver También

- Documentación oficial de Go para trabajar con archivos: https://golang.org/pkg/os/
- Go by Example ofrece ejemplos sencillos para manejo de archivos: https://gobyexample.com/writing-files
- Blog de Go sobre el paquete `io` en Go 1.16: https://blog.golang.org/go1.16#ioutil-package
