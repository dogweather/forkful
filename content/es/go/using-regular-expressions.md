---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué & Para qué?
Las expresiones regulares, o Regex, son patrones de texto que permiten buscar, encontrar y reemplazar ciertos textos en un string. Son una herramienta útil en la programación ya que ahorran tiempo y esfuerzo al trabajar con grandes volúmenes de datos.

## ¿Cómo hacerlo?
Para trabajar con Regex en Go, necesitaremos importar la biblioteca `regexp`. Aquí hay un ejemplo :

```Go 
package main 

import (
  "fmt"
  "regexp"
)

func main() {
  re := regexp.MustCompile(`a.b`)
    
  fmt.Println(re.MatchString("acb"))  // Devuelve True
  fmt.Println(re.MatchString("acb"))  // Devuelve False
}
```
En este código, buscamos un patrón donde la cadena tiene un 'a', seguido de cualquier carácter, seguido de un 'b'. El uso de '.' en la expresión regular representa cualquier carácter entre 'a' y 'b'.

## Profundizando 
Las expresiones regulares datan de los años 50 y se han implementado en diferentes lenguajes de programación desde entonces. En contextos en los que las operaciones de texto son más limitadas, otra opción podría ser usar métodos integrados de manipulación de string, como `strings.Replace`, `strings.Contains`, etc. Sin embargo, los Regex son más potentes y versátiles. En términos de implementación en Go, hay algo que debes recordar: si el patrón de la expresión regular no es válido, `regexp.MustCompile` provocará un panic.

## Más información
Para más información sobre las expresiones regulares en Go, puedes leer la [documentación oficial](https://golang.org/pkg/regexp/) o esta [guía](https://github.com/StefanSchroeder/Golang-Regex-Tutorial) en GitHub. Si estás interesado en saber más sobre las expresiones regulares y su teoría, [este libro](https://www.amazon.com/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596528124) puede ser útil.