---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Eliminar caracteres que coinciden con un patrón en la programación de Go significa borrar conjuntos específicos de caracteres en base a un patrón dado. Lo hacemos para limpiar, manipular datos y prepararlos para futuros procesos.

## ¿Cómo se hace?
Aquí tienes un sencillo extracto de código que utiliza la función `strings.TrimSpace` y `strings.Replace`:

```Go
package main
import (
   "strings"
   "fmt"
)

func main() {
   str := "   Hola, Mundo!   "
   str = strings.TrimSpace(str)
   str = strings.Replace(str, ",", "", -1)

   fmt.Println(str)
}
```

Resultando en:

```Go
"Hola Mundo!"
```

## Un Vistazo Más Profundo
Borrar caracteres que coinciden con un patrón tiene una historia interesante. Antes del surgimiento de las funciones incorporadas en lenguajes modernos como Go, los programadores tendrían que diseñar sus propias rutinas de manipulación de cadenas. Hoy en día, Go proporciona funciones incorporadas para hacer este trabajo.

Además de `strings.TrimSpace` y `strings.Replace`, puedes usar una expresión regular para borrar caracteres que coinciden con un patrón en Go. Aquí está el código de ejemplo:

```Go
package main
import (
    "regexp"
    "fmt"
)

func main() {
    re, _ := regexp.Compile("[0-9]")
    str := re.ReplaceAllString("h0l4 m3und0", "")
    fmt.Println(str) 
}
```

Este código eliminará todos los dígitos del string, sobrando:

```Go
"hla m undo"
```

Entender estos métodos te dará una flexibilidad significativa para manejar texto y datos en tus programas.

## Más Información
Para obtener más detalles y funciones útiles en Go, visita los siguientes enlaces: 
- Documentación oficial de Go sobre el paquete strings: https://golang.org/pkg/strings/
- Un simple tutorial de Go en español: https://go-tour-es.appspot.com/welcome/1
- Expresiones regulares en Go: https://gobyexample.com/regular-expressions