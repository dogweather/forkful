---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Go: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
La conversión de una cadena de texto a minúsculas es un proceso de modificación de todas las letras en la cadena a su forma de minúsculas. Los programadores lo hacen para estandarizar la entrada de texto y facilitar la comparación y el procesamiento de datos.

## Cómo hacerlo:
```Go
texto := "Hola, Este Es un Ejemplo DE texto"
fmt.Println(strings.ToLower(texto))

// Salida: hola, este es un ejemplo de texto
```

## Detalles de la Implementación
La función `ToLower()` de la biblioteca de cadenas de Go utiliza el estándar Unicode para convertir las letras a minúsculas. Esta función es una alternativa a la función `ToTitle()` que convierte las letras a su forma de mayúsculas.

## Ver también
- Documentación oficial de la función strings.ToLower(): https://golang.org/pkg/strings/#ToLower
- Tutorial sobre manipulación de cadenas en Go: https://www.golang-book.com/books/intro/9