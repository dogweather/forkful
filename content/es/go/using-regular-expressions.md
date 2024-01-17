---
title:                "Utilizando expresiones regulares"
html_title:           "Go: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Las expresiones regulares son un conjunto de patrones utilizados por los programadores para buscar y manipular cadenas de texto de manera eficiente. Esto permite realizar tareas como buscar y reemplazar texto en grandes cantidades de datos o validar el formato de una entrada de usuario. 

Los programadores usan expresiones regulares porque son una herramienta poderosa y versátil para trabajar con cadenas de texto. Con su capacidad para realizar búsquedas precisas y manipular datos de manera rápida, las expresiones regulares son una parte esencial de cualquier kit de herramientas de programación.

## ¡Manos a la obra!
En Go, podemos utilizar expresiones regulares con el paquete ```regexp``` que viene incorporado en el lenguaje. A continuación, se presentan algunos ejemplos de cómo podemos usar expresiones regulares en Go:

Para buscar una coincidencia exacta entre una cadena de texto y un patrón, podemos utilizar la función ```MatchString()``` que toma dos argumentos: el patrón a buscar y la cadena de texto a buscar en.

```
package main

import (
	"fmt"
	"regexp"
)

func main() {
	match, _ := regexp.MatchString("go", "go programming is awesome")
	fmt.Println(match)
	// Output: true
}
```

Para buscar más de una coincidencia en una cadena de texto, podemos usar la función ```FindAllString()```, que toma también dos argumentos, pero devuelve un slice de todas las coincidencias encontradas.

```
package main

import (
	"fmt"
	"regexp"
)

func main() {
	r := regexp.MustCompile("[0-9]+")
	fmt.Println(r.FindAllString("There are 5 apples and 12 oranges", 2))
	// Output: [5, 12]
}
```

## En profundidad
Las expresiones regulares tienen una larga historia en la informática, y se remontan a la década de 1950. Aunque hay muchas formas diferentes de trabajar con cadenas de texto en el mundo de la programación, las expresiones regulares siguen siendo una herramienta valiosa para aquellos que buscan una solución rápida y elegante para trabajar con grandes cantidades de datos de texto.

Aunque Go tiene su propio paquete ```regexp``` incorporado, también existen otras bibliotecas y frameworks populares como PCRE y Perl Compatible Regular Expressions. Sin embargo, el paquete ```regexp``` de Go es muy eficiente y fácil de usar, por lo que siempre es una buena opción para aquellos que trabajan con Go.

## Ver también
- [Documentación oficial de Go sobre expresiones regulares] (https://golang.org/pkg/regexp/)
- [Tutorial interactivo sobre expresiones regulares en Go] (https://regex-golang.appspot.com/assets/html/index.html)
- [Repositorio de expresiones regulares de Go] (https://github.com/golang/example/tree/master/regexp)