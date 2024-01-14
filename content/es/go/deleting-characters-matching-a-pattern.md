---
title:                "Go: Eliminar caracteres que coinciden con un patrón."
simple_title:         "Eliminar caracteres que coinciden con un patrón."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

La eliminación de caracteres que coinciden con un patrón es una tarea común en la programación. Puede ser útil para limpiar una cadena de texto antes de realizar operaciones adicionales en ella. En esta publicación de blog, aprenderemos cómo hacerlo utilizando el lenguaje de programación Go.

## Cómo hacerlo

La eliminación de caracteres en Go es posible gracias al paquete "strings". Dentro de este paquete, hay una función llamada "ReplaceAll" que elimina todos los caracteres que coinciden con un patrón especificado en una cadena de texto. Veamos un ejemplo de cómo se utiliza esta función:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "¡Hola, Go es increíble Go!"
    newText := strings.ReplaceAll(text, "Go", "")
    fmt.Println(newText)
}
```

En este ejemplo, tenemos una cadena de texto con dos repeticiones de la palabra "Go". La función "ReplaceAll" eliminará ambas repeticiones y nos devolverá la cadena de texto original sin esas palabras. La salida de este código sería:

```
¡Hola, es increíble!
```

## Profundizando

La función "ReplaceAll" es útil para la eliminación de caracteres, pero también existen otras funciones en el paquete "strings" que pueden ser utilizadas para este propósito. Por ejemplo, "Trim" elimina los caracteres especificados del principio y el final de una cadena de texto. "TrimLeft" y "TrimRight" hacen lo mismo, pero solo eliminan los caracteres del lado izquierdo o derecho de la cadena.

También es posible utilizar expresiones regulares para eliminar caracteres que coinciden con un patrón específico. Para hacerlo, podemos importar el paquete "regexp" y utilizar la función "ReplaceAllString" para reemplazar los caracteres coincidentes con una expresión regular. Este enfoque es útil si necesitamos eliminar caracteres más complejos de una cadena de texto.

## Ver también

- Documentación oficial sobre el paquete "strings" en Go: https://golang.org/pkg/strings/
- Ejemplos de uso de expresiones regulares en Go: https://gobyexample.com/regular-expressions
- Una guía paso a paso sobre cómo utilizar la función "ReplaceAll" en Go: https://www.dotnetperls.com/replace-go