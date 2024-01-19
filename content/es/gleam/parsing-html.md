---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Digamos que te encuentras frente a un gran bloque de HTML, marcado con elementos, atributos y texto. Analizar HTML es el proceso de transformar esa masa de texto en una estructura de datos manejable. Los programadores analizan HTML para extraer información útil, para la manipulación de la web o para la extracción de datos.

## ¿Cómo se hace?
```Gleam
import gleam/httpc

fn to_html_document(resp: httpc.Response) -> Result(html.Document, html.Error) {
  html.Document.from_string(resp.body)
}

fn main() {
  let resp = httpc.get(from: "http://example.com").unwrap()
  let doc = to_html_document(resp).unwrap()

  let paragraphs = doc
    |> html.select(["html", "body", "p"])
    |> result.unwrap()
    
  paragraphs
    |> list.map(fn(p) {
      p
       |> html.get_text
       |> result.unwrap()
    })
}
```
En este ejemplo, cargamos http://example.com, lo convertimos en un documento HTML y seleccionamos todas las etiquetas de párrafo en el documento. Posteriormente, obtenemos texto plano de las etiquetas de párrafo.

## Inmersión Profunda
Históricamente, el análisis de HTML ha sido un proceso engorroso y propenso a errores debido a su carácter altamente flexible y su tolerancia a errores. Gleam simplifica este proceso gracias a su manejo de tipos y gestión de errores robusta.

Además de los métodos de análisis HTML proporcionados directamente por Gleam, existen varias alternativas como BeautifulSoup en Python o Cheerio en Node.js, que cada uno tiene su propia interfa¿z y estilo.

Detalles de la implementación: el módulo `html` de Gleam utiliza hoja de ruta personalizada para analizar HTML. Cuando analizas HTML con Gleam, estás construyendo un árbol DOM (Document Object Model) que luego puedes manipular y consultar.

## Ver También
Puedes consultar la documentación oficial de Gleam [aquí](https://gleam.run/book/tour/data-types.html). Te recomendamos también ver la documentación de `html` de Gleam [aquí](https://github.com/gleam-lang/stdlib/tree/main/src/gleam/html.gleam).