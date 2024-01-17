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

## ¿Qué y por qué?
Eliminar caracteres que coinciden con un patrón es una técnica común en la programación para filtrar y manipular cadenas de texto. Los programadores lo hacen para limpiar datos, validar entradas o realizar transformaciones específicas en sus programas.

## ¿Cómo hacerlo?
En Go, podemos eliminar caracteres que coinciden con un patrón utilizando el paquete "regexp", que nos permite trabajar con expresiones regulares. Aquí hay un ejemplo de cómo eliminar todas las vocales de una cadena de texto:

```Go
import "regexp"

func main() {
    input := "Hola mundo!"
    output := regexp.MustCompile("[aeiou]").ReplaceAllString(input, "")
    // output: Hl mnd!
}
```

El código anterior utiliza la función "ReplaceAllString" del paquete "regexp" para reemplazar todas las vocales (que coincidan con el patrón "[aeiou]") en la cadena de entrada con una cadena vacía, eliminándolas así del resultado final.

## Profundizando
La utilización de expresiones regulares para eliminar caracteres coincidentes es una técnica muy versátil y poderosa, ya que permite especificar patrones complejos y realizar manipulaciones avanzadas en cadenas de texto. Además, en Go también podemos hacer uso del paquete "strings" para realizar elimaciones más simples basadas en igualdad de caracteres, sin necesidad de utilizar expresiones regulares.

## Véase también
- [Documentación oficial de Go: regexp](https://golang.org/pkg/regexp/)
- [Especificación de expresiones regulares en Go](https://golang.org/src/regexp/syntax/syntax.go)
- [Documentación oficial de Go: strings](https://golang.org/pkg/strings/)
- [Tutoriales de expresiones regulares en Go](https://www.calhoun.io/using-regex-in-go/)