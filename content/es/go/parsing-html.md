---
title:                "Análisis de HTML"
date:                  2024-01-20T15:31:49.833744-07:00
html_title:           "Arduino: Análisis de HTML"
simple_title:         "Análisis de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/parsing-html.md"
---

{{< edit_this_page >}}

## Qué es y Por qué?
Parsear HTML significa analizar el contenido de un documento HTML para poder manipularlo o acceder a su información. Programadores lo hacen para interactuar con la web, extraer datos, manipular páginas y automatizar tareas en línea.

## Cómo hacerlo:
Vamos a utilizar el paquete `goquery`, que es una biblioteca inspirada en jQuery para parsear HTML. Primero, necesitas instalarlo:
```bash
go get github.com/PuerkitoBio/goquery
```

Ahora, mira cómo cargar un documento HTML y buscar un elemento:

```Go
package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/PuerkitoBio/goquery"
)

func main() {
	// Obtener HTML de un sitio web
	res, err := http.Get("https://es.wikipedia.org/wiki/Go_(lenguaje_de_programaci%C3%B3n)")
	if err != nil {
		log.Fatal(err)
	}
	defer res.Body.Close()

	if res.StatusCode != 200 {
		log.Fatalf("status code error: %d %s", res.StatusCode, res.Status)
	}

	// Crear un documento goquery a partir del HTML
	doc, err := goquery.NewDocumentFromReader(res.Body)
	if err != nil {
		log.Fatal(err)
	}

	// Buscar elementos en el documento
	doc.Find(".mw-parser-output p").Each(func(i int, s *goquery.Selection) {
		// Imprimir el texto de cada párrafo
		fmt.Println("- ", s.Text())
	})
}
```

Ejecuta el programa y verás como salida un listado de párrafos del artículo de Wikipedia sobre Go.

## Profundizando

**Contexto histórico**: La necesidad de parsear HTML surgió poco después de la creación de la web. Las aplicaciones se volvieron más dinámicas, y el análisis del HTML pasó de ser una curiosidad a un requisito para aplicaciones web complejas.

**Alternativas**: además de `goquery`, puedes usar otras bibliotecas como `colly` para scraping, o el paquete estándar que ofrece Go, `html/template`, para parsear HTML con un enfoque más centrado en plantillas. Sin embargo, `goquery` es muy popular por su facilidad de uso y su poderosa sintaxis similar a jQuery.

**Detalles de implementación**: `goquery` permite navegar y manipular estructuras de nodos HTML. Usa el paquete `net/html` de Go para parsear documentos y ofrece una forma idiomática de trabajar con el DOM en Go.

## Ver También

- Documentación de GoQuery: [https://pkg.go.dev/github.com/PuerkitoBio/goquery](https://pkg.go.dev/github.com/PuerkitoBio/goquery)
- Proyecto Colly para scraping en Go: [http://go-colly.org/](http://go-colly.org/)
- Go html/template package: [https://golang.org/pkg/html/template/](https://golang.org/pkg/html/template/)
