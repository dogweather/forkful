---
title:                "Go: Cambiando a mayúsculas una cadena"
simple_title:         "Cambiando a mayúsculas una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué
El capitalizar una cadena de texto es una funcionalidad muy útil en cualquier lenguaje de programación. En Go, nos permite cambiar la capitalización de una cadena de caracteres según nuestras necesidades.

## Cómo hacerlo
Para capitalizar una cadena en Go, podemos utilizar la función `strings.ToUpper()`. Veamos un ejemplo práctico:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    frase := "hoy es un buen día para programar"
    
    fmt.Println(strings.ToUpper(frase))
}
```

**Salida:**
```
HOY ES UN BUEN DÍA PARA PROGRAMAR
```

En este ejemplo, utilizamos la función `ToUpper()` del paquete `strings` para convertir nuestra cadena `frase` en mayúsculas. También podemos utilizar la función `strings.ToLower()` para convertirla en minúsculas.

Otra opción es utilizar la función `strings.Title()`, que capitaliza la primera letra de cada palabra en una cadena. Veamos un ejemplo:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    titulo := "aprendiendo a programar en go"
    
    fmt.Println(strings.Title(titulo))
}
```

**Salida:**
```
Aprendiendo A Programar En Go
```

## Profundizando
Si queremos trabajar con cadenas de caracteres más complejas, como por ejemplo acentos o caracteres especiales, es importante tener en cuenta que la función `ToUpper()`, `ToLower()` y `Title()` no siempre funcionarán correctamente. Es por eso que existen otras funcionalidades más avanzadas en Go para manejar capitalizaciones específicas.

Una de ellas es el paquete `golang.org/x/text/` que nos brinda una gran variedad de funcionalidades para trabajar con cadenas de caracteres en diferentes idiomas. Este paquete utiliza el estándar Unicode para manejar caracteres especiales y acentuados.

## Véase también
- [Documentación oficial de la función `ToUpper()` en la página de Go](https://golang.org/pkg/strings/#ToUpper)
- [Documentación oficial del paquete `golang.org/x/text/`](https://pkg.go.dev/golang.org/x/text)
- [Ejemplos de uso de la función `Title()` en la documentación de Go](https://golang.org/pkg/strings/#Title)