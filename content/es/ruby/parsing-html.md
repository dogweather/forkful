---
title:                "Análisis de HTML"
date:                  2024-01-20T15:33:44.882302-07:00
html_title:           "Arduino: Análisis de HTML"
simple_title:         "Análisis de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por Qué?
Parsear HTML significa analizar y extraer datos de documentos HTML. Los programadores lo hacen para automatizar la recopilación de información de sitios web, algo conocido como web scraping.

## Cómo hacerlo:
Para parsear HTML en Ruby, necesitarás una gema como `Nokogiri`, que hace el trabajo pesado por ti. Primero, instala la gema con `gem install nokogiri`, y luego echa un vistazo al siguiente código:

```Ruby
require 'nokogiri'
require 'open-uri'

# Obtener HTML desde una URL
html = open('https://www.ejemplo.com')
doc = Nokogiri::HTML(html)

# Buscar elementos por CSS
titulos = doc.css('h1')
titulos.each { |titulo| puts titulo.content }

# Output muestra el texto de cada elemento <h1> encontrado
```

Con esto, obtendrías el contenido de cada `<h1>` del documento HTML.

## Deep Dive:
Parsear HTML no es nuevo. En la década de los 90, con el auge de la web, se hizo esencial. Alternativas a Nokogiri incluyen `Oga` y `Mechanize`. Mientras `Mechanize` automatiza interacciones web, `Oga` es sencilla y no depende de extensiones nativas, a diferencia de `Nokogiri`, que usa `libxml2`.

Cuando parseas HTML, necesitas distinguir entre dos cosas: el árbol DOM y cómo acceder a él. `Nokogiri` lee el HTML y crea un DOM, que puedes navegar usando métodos similares a los de un navegador web.

## Ver También:
- Documentación de Nokogiri: [http://nokogiri.org/](http://nokogiri.org/)
- Ruby Mechanize: [https://github.com/sparklemotion/mechanize](https://github.com/sparklemotion/mechanize)
- Ruby Oga: [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
