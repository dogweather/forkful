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

## ¿Qué & ¿Por qué?
El HTML es el lenguaje utilizado para crear y presentar contenido en la web. Al parsear HTML, los programadores pueden analizar y extraer información específica de una página web para utilizarla en su propio código. Esto es especialmente útil para la automatización de tareas y para obtener datos de fuentes externas.

## ¿Cómo hacerlo?
Utilizando el lenguaje de programación Go, podemos implementar un parser de HTML bastante sencillo. Primero, importamos la biblioteca de Go llamada "html" y luego utilizamos la función "Parse" para analizar el contenido HTML y devolver una estructura de datos que podamos manipular. Aquí hay un ejemplo de código que extrae todos los hipervínculos de una página web y los imprime en la consola:

```Go
package main

import (
    "fmt"
    "log"

    "golang.org/x/net/html"
)

func main() {
    // Recuperar la página web
    resp, err := http.Get("http://www.example.com")
    if err != nil {
        log.Fatal(err)
    }
    defer resp.Body.Close()

    // Analizar contenido HTML
    doc, err := html.Parse(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // Función para recorrer árbol HTML y encontrar hipervínculos
    var findLinks func(*html.Node)
    findLinks = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            findLinks(c)
        }
    }
    findLinks(doc)
}
```

Este código utiliza una función recursiva para recorrer cada nodo en el árbol HTML y buscar elementos "a" (hipervínculos) y sus atributos "href" (la dirección URL). Luego, utiliza la función de impresión de Go para mostrar cada hipervínculo encontrado en la consola.

## Profundizando
El parsing de HTML ha sido un tema importante en la creación de aplicaciones web desde los inicios de la web en 1990. Antes de Go, los programadores utilizaban principalmente lenguajes como Python y Java para realizar tareas de scraping o extracción de datos de la web. Sin embargo, con la llegada de Go, se ha vuelto más fácil y rápido implementar esta funcionalidad.

Hay varias bibliotecas de Go disponibles para el parsing de HTML, cada una con sus propias características y fortalezas. Algunas alternativas populares son "goquery" y "selectors". También es posible utilizar otras herramientas como "Regular Expressions" o "Xpath" para el parsing de HTML, pero estas opciones pueden resultar más complicadas.

El paquete "html" de Go también ofrece varias funciones adicionales para manipular y trabajar con el árbol HTML, como "Attr", "Tag", "Parent" y más. Estas pueden ser útiles para tareas más específicas o avanzadas de parsing.

## Ver también
Para obtener más información sobre el parsing de HTML con Go, se pueden consultar las siguientes fuentes:

- Documentación oficial de Go sobre el paquete "html": https://golang.org/pkg/html/
- Ejemplos de código en el sitio de Go por la comunidad: https://gobyexample.com/html-parsing
- Tutorial en video sobre parsing de HTML en Go: https://www.youtube.com/watch?v=Cy2HgH0OwXU