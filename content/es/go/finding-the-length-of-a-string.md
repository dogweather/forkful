---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Encontrar la longitud de una cadena se refiere a determinar el número de caracteres en la cadena. Los programadores a menudo necesitan esta información para validar entradas de usuario o para controlar bucles.

## Cómo se hace:

A continuación se presenta un código sencillo de Go que muestra cómo usar el método `len()` para encontrar la longitud de una cadena.

```Go
package main
import "fmt"

func main() {
    cadena := "Hola Mundo"
    fmt.Println(len(cadena))
}
```

Cuando se ejecuta este código, produce la siguiente salida:

```
10
```

Este resultado significa que la cadena "Hola Mundo" tiene diez caracteres.

## Un Vistazo más Profundo

Antiguamente, encontrar la longitud de una cadena no era tan sencillo. Los antiguos lenguajes de programación requerían recorrer la cadena, carácter por carácter, y contarlos. Afortunadamente, Go incorporó la función `len()` desde el principio, facilitando la tarea.

Alternativamente, puedes usar la función `utf8.RuneCountInString()`, si necesitas considerar caracteres Unicode que pueden ocupar más de un byte. Esta función también es proporcionada por Go. Aquí tienes cómo se usaría:

```Go
package main
import (
	"fmt"
	"unicode/utf8"
)

func main() {
	cadena := "¡Hola, Mundo!"
	fmt.Println(utf8.RuneCountInString(cadena))
}
```

La implementación de `len()` en Go es muy eficiente, ya que no necesita recorrer la cadena, sino que simplemente devuelve un valor almacenado en la estructura de datos de la cadena.

## Ver También

Para más información sobre la programación en Go y las cadenas de caracteres, consulta los siguientes enlaces:

- Documentación oficial de Go: https://golang.org/doc/
- Tutorial de Go sobre cadenas: https://gobyexample.com/strings
- Pregunta sobre la longitud de la cadena en Go en StackOverflow: https://stackoverflow.com/questions/12668681/how-to-get-the-number-of-characters-in-a-string

Recuerda, la práctica hace al maestro. ¡Sigue programando!