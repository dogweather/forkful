---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Go: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué eliminar caracteres coincidentes con un patrón?

A veces, queremos limpiar una cadena de caracteres de ciertos caracteres que siguen un patrón específico. En lugar de hacerlo manualmente, podemos usar una función en Go que nos permita eliminar automáticamente estos caracteres para ahorrar tiempo y evitar errores.

## Cómo hacerlo

Para eliminar caracteres coincidentes con un patrón en Go, podemos utilizar la función `Trim()` de la biblioteca `strings`. Esta función toma dos parámetros: la cadena de caracteres a limpiar y una cadena de caracteres que contiene el patrón a eliminar.

Ejemplo de código:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "¡H0la mundo!"
    cleanStr := strings.Trim(str, "0")
    fmt.Println(cleanStr)
}
```

Este código producirá la salida `¡Hla mundo!`, ya que el patrón "0" ha sido eliminado de la cadena de caracteres original.

Otro ejemplo:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "This123 is456 a78 test"
    cleanStr := strings.Trim(str, "1234567890")
    fmt.Println(cleanStr)
}
```

En este caso, la salida sería `This is a test`, ya que todos los números han sido eliminados de la cadena original.

## Profundizando

La función `Trim()` también puede ser combinada con otras funciones como `ReplaceAll()` para eliminar patrones más complejos o reemplazarlos con otros caracteres. Además, esta funcionalidad también se puede aplicar a otras estructuras de datos como slices y maps.

Para obtener más información sobre cómo utilizar la función `Trim()` de manera más avanzada, se recomienda revisar la documentación oficial de Go y explorar otros ejemplos de código.

## Ver también

- [Documentación oficial de Go](https://golang.org/doc/)
- [Tutorial de funciones de cadenas en Go](https://www.calhoun.io/6-tips-for-using-strings-in-go/)