---
title:                "Go: Parsing de html"
simple_title:         "Parsing de html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías aprender a analizar HTML en Go?

Entender cómo analizar y manipular HTML puede ser una habilidad muy útil para los desarrolladores de Go. Una de las razones por las cuales es importante es que permite automatizar tareas repetitivas y extraer información específica de una página web. También puede ser útil para la creación de aplicaciones web y el scraping de datos.

## Cómo hacerlo en Go

Para analizar HTML en Go, necesitarás importar el paquete "html" y utilizar la función "Parse" para convertir el HTML en una estructura de nodos. Luego, puedes recorrer esta estructura utilizando un bucle para acceder a los diferentes elementos y sus atributos. Aquí hay un ejemplo de código para obtener el título de una página web:

```Go
package main

import (
	"fmt"
	"net/http"
	"strings"

	"golang.org/x/net/html"
)

func main() {
	resp, err := http.Get("https://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	doc, err := html.Parse(resp.Body)
	if err != nil {
		panic(err)
	}
	fmt.Println(findTitle(doc))
}

func findTitle(n *html.Node) string {
	if n.Type == html.ElementNode && n.Data == "title" {
		return strings.TrimSpace(n.FirstChild.Data)
	}
	for i := n.FirstChild; i != nil; i = i.NextSibling {
		title := findTitle(i)
		if title != "" {
			return title
		}
	}
	return ""
}
```

El código anterior accede a la página web de ejemplo y busca el elemento "title". Luego, imprime el contenido del elemento en la consola. Puedes utilizar este mismo enfoque para acceder a otros elementos y realizar diferentes acciones con ellos.

## Profundizando en el análisis de HTML

El paquete "html" de Go ofrece muchas otras funciones útiles para trabajar con HTML. Puedes usar "NewTokenizer" para analizar el HTML de forma más eficiente, utilizar la función "NewDecoder" para decodificar contenido con caracteres especiales y utilizar el paquete "strings" para manipular cadenas de texto.

Además, también puedes buscar y seleccionar elementos utilizando selectores CSS a través del paquete "goquery". Este paquete hace que sea más fácil y sencillo trabajar con HTML y permite escribir código más legible y mantenible.

## Ver también

- [Golang.org/x/net/html package](https://pkg.go.dev/golang.org/x/net/html)
- [Go Query library](https://github.com/PuerkitoBio/goquery)
- [Tutorial: Introducción al web scraping con Go](https://blog.golang.org/web-scraping-with-go)