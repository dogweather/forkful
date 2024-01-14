---
title:    "Go: Leyendo argumentos de línea de comando"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comandos en Go?

Los argumentos de línea de comandos son una forma útil de interactuar con programas y aplicaciones en el sistema operativo. Al aprender a leer y utilizar los argumentos de línea de comandos en Go, podrás trabajar de manera más eficiente y realizar tareas específicas en tus proyectos.

## Cómo hacerlo

Para leer argumentos de línea de comandos en Go, simplemente debes utilizar la función `os.Args`. Esta función devuelve un slice (rebanada) de tipo `[]string` con todos los argumentos de línea de comandos pasados al programa.

Veamos un ejemplo de cómo imprimir los argumentos en la terminal:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args[1:] // Ignora el primer argumento que se refiere al nombre del programa itself (sí mismo)
    fmt.Println("Argumentos pasados:")
    for _, arg := range args {
        fmt.Println(" - " + arg)
    }
}
```
Output:
```
➜ go run main.go hello world
Argumentos pasados:
 - hello
 - world 
```

## Profundizando

Hay varias maneras en las que puedes utilizar los argumentos de línea de comandos en tus proyectos. Puedes validarlos, leer diferentes tipos de datos y también utilizar librerías externas para facilitar su uso.

Además, es importante mencionar que los argumentos de línea de comandos pueden incluir flags (banderas) y opciones, que te permiten pasar información adicional y personalizar el comportamiento del programa.

Si deseas explorar más sobre cómo trabajar con argumentos de línea de comandos en Go, te recomendamos revisar la documentación oficial y experimentar con diferentes ejemplos.

## Ver también

- Documentación oficial de Go sobre os.Args: https://golang.org/pkg/os/#Args
- Ejemplos de uso de argumentos de línea de comandos en Go: https://github.com/golang/example/tree/master/cli