---
title:                "Capitalizando una cadena"
html_title:           "Go: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Al programar en Go, puede ser necesario capitalizar una cadena, es decir, convertir todas las letras en mayúsculas. Esto es comúnmente utilizado para mejorar la legibilidad de la cadena o para comparar dos cadenas sin tener en cuenta las mayúsculas y minúsculas. Los programadores hacen esto para facilitar el procesamiento y la manipulación de cadenas en su código.

## ¿Cómo hacerlo?

Para capitalizar una cadena en Go, podemos utilizar la función `ToUpper` del paquete `strings`. Esta función toma una cadena como argumento y devuelve una nueva cadena con todas las letras en mayúsculas. Veamos un ejemplo:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hola mundo"
    fmt.Println(strings.ToUpper(str))
}
```

El resultado de este código sería "HOLA MUNDO".

## Profundizando

La capitalización de cadenas es una técnica comúnmente utilizada en la programación y existen varias formas de hacerlo en Go. Además de `ToUpper`, también podemos utilizar la función `Title` del paquete `strings`, que convierte la primera letra de cada palabra en mayúscula. Por ejemplo:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hola mundo"
    fmt.Println(strings.Title(str))
}
```

El resultado sería "Hola Mundo". También es importante tener en cuenta que la función `ToUpper` y `Title` devuelven una nueva cadena, por lo que la cadena original no se ve afectada por la operación.

## Ver también

- [Documentación oficial de la función ToUpper en Go](https://golang.org/pkg/strings/#ToUpper)
- [Blog sobre el uso de cadenas en Go](https://blog.golang.org/strings)