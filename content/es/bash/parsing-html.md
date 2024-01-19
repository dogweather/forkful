---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
El análisis de HTML (parsing HTML) se refiere a desglosar el contenido de una página web y extraer información valiosa de ella. Los programadores llevan a cabo el análisis de HTML para obtener datos de sitios web, como información de productos, precios, descripciones, comentarios de usuarios, etc.

## Cómo hacerlo:

Para analizar documentos HTML en Bash, podemos usar herramientas como `pup`, `lynx`, `w3m`, `hxnormalize`, etc. Aquí utilizaremos 'pup'.

Instale 'pup' de la siguiente manera:

```bash
go get github.com/ericchiang/pup
```
Extracción de los títulos de los artículos de Hacker News:

```bash
curl -s https://news.ycombinator.com/ | pup 'a.storylink text{}'
```
Este comando devolverá todos los títulos de los artículos en la página principal de Hacker News.

## Inmersión profunda:

El análisis de HTML no es algo nuevo. Fue muy popular durante la época en que los motores de búsqueda aún estaban en su infancia. Aunque las APIs y los datos estructurados han reemplazado en gran medida al antiguo método de extracción de datos, el análisis de HTML sigue siendo una habilidad valiosa.

Existen alternativas a 'pup' como 'BeautifulSoup' y 'Scrapy' en Python, 'Cheerio' en Javascript, etc.

'Pup' implementa selectores de CSS para filtrar elementos HTML. Mira el archivo HTML como un conjunto de cajas anidadas (el modelo de caja de CSS) y proporciona maneras de seleccionarlas y manipularlas.

## Ver también:

1. Documentación de 'Pup': https://github.com/ericchiang/pup
2. Análisis de HTML con Python: https://docs.python-guide.org/scenarios/scrape/
3. El DOM y CSS Selectors: http://www.w3schools.com/js/js_htmldom.asp, https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Getting_started/Selectors
4. Análisis de HTML con JavaScript: https://cheerio.js.org/