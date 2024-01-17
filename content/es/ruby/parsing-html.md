---
title:                "Análisis de HTML"
html_title:           "Ruby: Análisis de HTML"
simple_title:         "Análisis de HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Parsing HTML es el proceso de interpretar y analizar el código HTML de una página web para extraer la información relevante de ella. Los programadores usan esto para crear aplicaciones que puedan automatizar tareas como extraer datos o hacer cambios en un gran número de páginas web de manera eficiente.

## Cómo hacerlo:
La siguiente es una manera sencilla de hacer un parsing básico de una página web en Ruby:

```Ruby
require 'open-uri'
require 'nokogiri'

url = "https://ejemplo.com"
html = open(url).read
parsed_html = Nokogiri::HTML(html)

# Extraer el título de la página
title = parsed_html.css('title').text
puts title
# Output: "Página de ejemplo"

# Extraer todos los enlaces en la página
links = parsed_html.css('a').map { |link| link['href'] }
puts links
# Output: ["https://ejemplo.com/pagina1.html", "https://ejemplo.com/pagina2.html", "https://ejemplo.com/pagina3.html"]
```

## Profundizando:
La práctica de parsing de HTML tiene sus raíces en los inicios de la World Wide Web, cuando la creación de páginas web todavía era un proceso manual. A medida que la web crecía, se volvió necesaria la automatización de ciertas tareas para hacerlas más eficientes. Alternativas al parsing HTML incluyen el uso de APIs o el acceso a bases de datos con información estructurada.

Para implementar el parsing en Ruby, existen varias gemas disponibles, como Nokogiri, Hpricot y Mechanize, cada una con sus propias características y ventajas.

## Ver también:
- Documentación oficial de Nokogiri: https://nokogiri.org/
- Tutorial de parsing HTML con Ruby: https://www.sitepoint.com/web-scraping-ruby/
- Gema Hpricot para parsing de HTML: https://github.com/hpricot/hpricot