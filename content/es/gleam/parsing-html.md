---
title:                "Analizando html"
html_title:           "Gleam: Analizando html"
simple_title:         "Analizando html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
El análisis de HTML es el proceso de interpretar y analizar el código HTML de una página web. Los programadores lo hacen para poder extraer información específica de una página, como por ejemplo los datos de un formulario o el contenido de una tabla.

## Cómo hacerlo:
¡Es fácil! En Gleam, podemos usar la biblioteca `Html.Parser` para analizar una página web. Primero, importamos la biblioteca con `import Html.Parser`. Luego, podemos usar la función `parse` para analizar el código HTML y obtener un árbol de nodos que representan la estructura de la página. Por ejemplo:

```
let tree = Html.Parser.parse(html_code)
tree
|> Html.Node.tag("table")
|> Html.Node.children
|> Html.Node.text
```

Este código analizará el código HTML y devolverá todos los elementos de texto dentro de la etiqueta `<table>` en un arreglo.

## Profundizando:
El análisis de HTML se ha vuelto cada vez más importante con el auge de la web y de las aplicaciones web. Algunas alternativas populares al análisis de HTML son las expresiones regulares y XPath. Sin embargo, estas no son tan robustas y flexibles como una biblioteca diseñada específicamente para analizar HTML.

La implementación de `Html.Parser` en Gleam está basada en la biblioteca `Html.parser` de Python, lo que garantiza una alta calidad y compatibilidad con otras bibliotecas de análisis de HTML.

## Véase también:
- [Biblioteca Html.Parser en Gleam](https://gleam.run/libraries/html_parser/)
- [Documentación de biblioteca Html.parser en Python](https://docs.python.org/3/library/html.parser.html)