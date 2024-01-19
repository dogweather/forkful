---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La interpolación de cadenas es un método para unir o incrustar variables en una cadena. Los programadores hacen esto para simplificar la manipulación de la cadena y mejorar la legibilidad.

## Cómo hacerlo:

Aquí mostramos un ejemplo.

```Go
package main

import(
	"fmt"
)

func main() {
	var nombre string = "Juan"
	fmt.Printf("¡Hola, %s!", nombre)
}
```
La salida de este programa será "¡Hola, Juan!". 

## Un Vistazo más Profundo:

La interpolación de strings tiene un largo recorrido en la historia de la programación; es útil y común en muchos lenguajes de programación, como Perl y Ruby.

En Go, puede utilizar la función `fmt.Sprintf()` como alternativa. Esta función devuelve una cadena en lugar de imprimir directamente la cadena. 

```Go
package main

import(
	"fmt"
)

func main() {
	var nombre string = "Juan"
	saludo := fmt.Sprintf("¡Hola, %s!", nombre)
	fmt.Println(saludo)
}
```
El programa de arriba también tiene la salida: "¡Hola, Juan!". 

## Ver También:

1. Documentación oficial de Go: [fmt](https://golang.org/pkg/fmt/)
2. Guía de programación en Go: [A Tour of Go](https://tour.golang.org/welcome/1) en español. 
3. String formatting in Go: [Go by Example](https://gobyexample.com/string-formatting).