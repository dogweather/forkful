---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La extracción de subcadenas en programación es el proceso de obtener un grupo específico de caracteres de una cadena existente. Los programadores lo hacen para manipular y trabajar con partes específicas de cadenas de texto, eliminando la necesidad de manejar la cadena completa.

## Cómo hacerlo:

Aquí verás cómo hacerlo en Go. Asumiremos que ya tienes conocimientos básicos del lenguaje.

```Go
package main

import "fmt"

func main() {
    str := "¡Hola Mundo!"
    sub := str[1:4]

    fmt.Println(sub)
}
```
La salida de este programa será "ola", que es la subcadena de los índices 1 al 4 en la cadena original "¡Hola Mundo!".

## Inmersión Profunda

Históricamente, la extracción de subcadenas ha sido utilizada en variedad de contextos de programación, desde la manipulación de cadenas para presentación visual hasta el procesamiento de texto para el análisis de datos.

En Go, existen varias formas para extraer subcadenas. La más directa y eficiente es la que se mostró anteriormente, usando el operador de rebanada `[:]`. Sin embargo, para operaciones más complejas, puedes utilizar funciones de la biblioteca estándar, como `strings.Split` y `strings.Trim`.

Recuerda que las cadenas en Go son inmutables: cuando extraes una subcadena, básicamente estás creando una nueva cadena. Go maneja este proceso de manera eficiente, pero es un detalle que debes tener en cuenta cuando trabajas con largas cadenas o realizas extracciones de subcadenas intensivas.

## Ver También

Para más detalles y ejemplos avanzados, puede consultar las siguientes fuentes:

- Documentación oficial de Go: [Paquete Strings](https://pkg.go.dev/strings)
- Específico para subcadenas, chequea [Go by Example: String functions](https://gobyexample.com/string-functions).