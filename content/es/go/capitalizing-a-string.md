---
title:    "Go: Capitalizando una cadena"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena de texto en Go?

Capitalizar una cadena de texto es un paso importante en cualquier programa que maneje datos ingresados por el usuario. Al capitalizar una cadena de texto, se asegura de que las palabras estén escritas correctamente y se facilita su procesamiento en el futuro.

## Cómo hacerlo en Go

En Go, el proceso de capitalizar una cadena de texto es muy sencillo. Primero, es necesario importar el paquete "strings" que contiene la función necesaria para capitalizar. Luego, simplemente se llama a la función "Title" y se pasa la cadena de texto que se desea capitalizar como argumento. A continuación, se muestra un ejemplo de código en Go y su correspondiente salida:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hola, ¿cómo estás?"
    fmt.Println(strings.Title(str))
}
```
Salida: "Hola, ¿Cómo Estás?"

## Profundizando en la capitalización de cadenas de texto

Cuando se capitaliza una cadena de texto, se debe tener en cuenta que se convierten todas las letras al formato de título, lo que significa que la primera letra de cada palabra se convierte en mayúscula y el resto en minúscula. Además, se debe tener en cuenta que los números y símbolos no se ven afectados por la capitalización.

Por ejemplo, si se tiene una cadena de texto "GO es genial!" y se aplica la función "Title", la salida será "Go Es Genial!".

Sin embargo, es importante señalar que esta función sólo capitaliza la primera letra de cada palabra en una cadena. Si se desea capitalizar todas las letras de una cadena de texto, se puede utilizar la función "ToUpper" del paquete "strings".

## Ver también

- [Documentación de la función Title de strings en la página oficial de Go](https://golang.org/pkg/strings/#Title)
- [Tutorial de Go en español para principiantes](https://tour.golang.org/welcome/1)