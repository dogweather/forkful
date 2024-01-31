---
title:                "Lectura de un archivo de texto"
date:                  2024-01-20T17:54:33.639972-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de un archivo de texto"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lectura de Archivos de Texto en Go: Lo Esencial y Más Allá

## ¿Qué & Por Qué?
Leer un archivo de texto es el acto de solicitar a tu programa que ingiera datos desde un archivo almacenado en tu disco duro. Hacemos esto porque, en la vida real, gran parte de la información que necesitamos procesar se encuentra en archivos, y queremos que nuestros programas interactúen con esos datos eficientemente.

## Cómo hacerlo:
Para leer un archivo de texto en Go, vamos a usar la librería estándar `io/ioutil`.

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
)

func main() {
    contenido, err := ioutil.ReadFile("ejemplo.txt")
    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("Contenido del archivo:\n%s", contenido)
}
```

**Salida de ejemplo:**
```plaintext
Contenido del archivo:
Hola, este es el texto de tu archivo.
```

## Inmersión Profunda
La lectura de archivos en Go tuvo su influencia de lenguajes predecesores como C y sus bibliotecas de I/O de archivo, pero está diseñada para ser más segura y menos propensa a errores. 

Historicamente, Go maneja la lectura de archivos usando varias funciones disponibles en el paquete `os`. Sin embargo, `io/ioutil` simplifica algunas tareas comunes como la lectura de todo el contenido de un archivo de una vez. Cabe mencionar que desde Go 1.16, las funciones de `io/ioutil` se consideran obsoletas y se recomienda usar `os` y `io`.

Aquí hay otra forma de leer un archivo de texto:

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "log"
)

func main() {
    archivo, err := os.Open("ejemplo.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer archivo.Close()

    scanner := bufio.NewScanner(archivo)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
    }
}
```

Mientras que `ioutil.ReadFile` es genial para leer archivos pequeños, `bufio.Scanner` es más adecuado para archivos grandes, ya que lee el archivo línea por línea, lo cual es más eficiente en memoria.

## Ver También
Para ampliar tu conocimiento con documentación oficial y otros recursos relacionados, aquí tienes algunos enlaces:

- Documentación oficial de Go: [Paquete io](https://pkg.go.dev/io) y [Paquete bufio](https://pkg.go.dev/bufio)
- Tutorial de Go sobre cómo leer archivos: [Tutorial de Golang](https://golangbot.com/read-files/)
