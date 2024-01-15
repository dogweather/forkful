---
title:                "Analizando html"
html_title:           "Go: Analizando html"
simple_title:         "Analizando html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez has querido extraer información específica de una página web? La respuesta se encuentra en el análisis de HTML. Ya sea para obtener datos de precios de productos en una tienda en línea o para recopilar información de noticias de un sitio de noticias, analizar HTML es una habilidad útil en el mundo de la programación.

## Cómo hacerlo
El lenguaje de programación Go ofrece una forma sencilla y eficiente de analizar HTML. Aquí hay un ejemplo de cómo podemos extraer todos los enlaces de una página web:

```go
package main

import (
    "fmt"
    "log"

    "golang.org/x/net/html"
)

func main() {
    // Obtén la página web que quieres analizar
    resp, err := http.Get("https://www.example.com")
    if err != nil {
        log.Fatal(err)
    }
    defer resp.Body.Close()

    // Analiza el HTML
    doc, err := html.Parse(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // Recorre todas las etiquetas <a> y obtén el valor del atributo "href"
    var links []string
    var c func(*html.Node)
    c = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    links = append(links, a.Val)
                }
            }
        }
        // Recorre los nodos secundarios
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            c(c)
        }
    }
    c(doc)

    // Imprime todos los enlaces recopilados
    for _, link := range links {
        fmt.Println(link)
    }
}
```

Este código utilizará la biblioteca "golang.org/x/net/html" para analizar el HTML de la página web. Primero, obtenemos la página con la función http.Get y luego la pasamos a html.Parse para obtener un árbol de nodos que representan el HTML de la página. Después, utilizamos una función recursiva para recorrer todos los nodos y obtener los enlaces de cada etiqueta <a>. Finalmente, imprimimos todos los enlaces recopilados.

## Profundizando
Si deseas profundizar en el análisis de HTML en Go, puedes explorar las opciones de la biblioteca "golang.org/x/net/html". Esta biblioteca te permite acceder a diferentes elementos del HTML, como el texto dentro de las etiquetas, los atributos y los nodos secundarios. También puedes combinar el análisis de HTML con el análisis de código CSS utilizando la biblioteca "github.com/PuerkitoBio/goquery". Con estas herramientas, puedes crear aplicaciones más sofisticadas para analizar y manipular páginas web.

## Ver también
- Documentación oficial de la biblioteca "golang.org/x/net/html": https://godoc.org/golang.org/x/net/html
- Documentación de la biblioteca "github.com/PuerkitoBio/goquery": https://godoc.org/github.com/PuerkitoBio/goquery