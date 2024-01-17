---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Go: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Leer argumentos de línea de comandos es simplemente obtener información que ingresa un usuario en la terminal al ejecutar un programa. Los programadores necesitan esto para poder personalizar y modificar la ejecución de su código según los valores provistos por el usuario.

## Cómo:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Leemos los argumentos proporcionados por el usuario y los almacenamos en una variable.
    args := os.Args
    // Iteramos a través de los argumentos y los imprimimos uno por uno.
    for i, arg := range args {
        fmt.Printf("Argumento #%d: %s\n", i+1, arg)
    }
}
```

Ejemplo de entrada:

```bash
go run main.go hola mundo 123
```

Salida:

```
Argumento #1: main.go
Argumento #2: hola
Argumento #3: mundo
Argumento #4: 123
```

## Profundizando:

La lectura de argumentos de línea de comandos ha sido una funcionalidad esencial desde los inicios de la programación. Además de obtener datos del usuario, también puede ser utilizada en sistemas operativos para proveer información al programa sobre cómo debe ser ejecutado.

En Go, la forma más común de leer argumentos es utilizando la función `Args()` del paquete `os`. Sin embargo, también existen otras opciones como el uso de la biblioteca `flag` o el módulo `os/exec` para ejecutar comandos externos y leer sus resultados.

## Vea también:

- [Documentación oficial de Golang sobre lectura de argumentos de línea de comandos](https://golang.org/pkg/os/#Args)
- [Guía de la comunidad de Golang sobre el uso de la biblioteca `flag`](https://gobyexample.com/command-line-flags)
- [Artículo de blog sobre el módulo `os/exec` para ejecutar comandos externos en Go](https://blog.golang.org/execing-child-processes)