---
title:                "Go: Borrando caracteres que coinciden con un patrón"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué eliminar caracteres que coincidan con un patrón?

A veces, en programación, necesitamos eliminar caracteres específicos de una cadena de texto que coincidan con un cierto patrón. Por ejemplo, puede que queramos eliminar todos los dígitos de una cadena de números o eliminar todos los caracteres no alfanuméricos de una cadena. En este blog post, aprenderemos cómo hacerlo en el lenguaje de programación Go.

## Cómo hacerlo

La forma más simple de eliminar caracteres que coincidan con un patrón en Go es utilizando el paquete `strings` y su función `ReplaceAll`. Esta función toma tres parámetros: la cadena original, el patrón a eliminar y la cadena vacía. A continuación, un ejemplo de cómo eliminar todos los dígitos de una cadena:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    cadena := "H0l4 Mund0"
    cadena = strings.ReplaceAll(cadena, "0", "")
    cadena = strings.ReplaceAll(cadena, "4", "")
    fmt.Println(cadena)
}
```

La salida de este ejemplo sería:

```
Hl Mund
```

También podemos utilizar expresiones regulares para eliminar patrones más complejos. El paquete `regexp` nos permite utilizar expresiones regulares en Go. A continuación, un ejemplo de cómo eliminar todos los caracteres no alfanuméricos de una cadena:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    cadena := "¡¡H0l4 MUNDO!!"
    regex := regexp.MustCompile("[^a-zA-Z0-9]+")
    cadena = regex.ReplaceAllString(cadena, "")
    fmt.Println(cadena)
}
```

La salida de este ejemplo sería:

```
H0l4MUNDO
```

## Inmersión profunda

En la sección anterior, solo vimos ejemplos básicos de cómo eliminar caracteres que coincidan con un patrón en Go. Pero el paquete `strings` y el paquete `regexp` tienen muchas más funciones útiles para manejar y manipular cadenas de texto. También hay otros paquetes que pueden ayudarnos a eliminar caracteres según un patrón determinado, como `strconv` (para manipular cadenas numéricas) y `unicode` (para manipular caracteres Unicode).

Es importante tener en cuenta que, al eliminar caracteres de una cadena, no estamos modificando la cadena original. En cambio, estamos creando una nueva cadena con los cambios aplicados. Esto se debe a que las cadenas son inmutables en Go (no pueden ser modificadas).

## Ver también

- [Paquete strings en la documentación de Go](https://golang.org/pkg/strings/)
- [Paquete regexp en la documentación de Go](https://golang.org/pkg/regexp/)
- [Paquete strconv en la documentación de Go](https://golang.org/pkg/strconv/)
- [Paquete unicode en la documentación de Go](https://golang.org/pkg/unicode/)