---
title:                "Analizando HTML"
date:                  2024-02-03T19:12:50.713841-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analizando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Analizar HTML significa desglosar un trozo de código HTML para comprender su estructura y contenido. Los programadores lo hacen para extraer datos, manipular contenido o migrar información entre formatos y sistemas.

## Cómo hacerlo:
Para analizar HTML en Ruby, instala la gema 'Nokogiri' con `gem install nokogiri`. Nokogiri es como una navaja suiza para trabajar con HTML y XML en Ruby. He aquí un ejemplo rápido:

```ruby
require 'nokogiri'
require 'open-uri'

# Cargar el contenido HTML de un sitio web
html_content = URI.open('http://example.com').read

# Analizar el HTML
doc = Nokogiri::HTML(html_content)

# Extraer el título
title = doc.xpath('//title').text
puts "El título de la página es: #{title}"
```

Esto devuelve algo como: `El título de la página es: Dominio de Ejemplo`.

## Inmersión Profunda
En los primeros días de Ruby, las opciones para analizar HTML eran limitadas. REXML estaba incorporado pero era lento. Luego apareció Hpricot, pero terminó desvaneciéndose. Nokogiri debutó en 2008, combinando la facilidad de Hpricot con la velocidad y el poder de libxml, un conjunto de herramientas XML probado.

En el mundo del análisis, siempre hay alternativas. Algunos juran por la biblioteca incorporada 'rexml' o 'oga', otro analizador de XML/HTML para Ruby. Pero Nokogiri sigue siendo el favorito por su robustez y velocidad, sin mencionar su vasta gama de características.

Bajo el capó, Nokogiri convierte HTML en un Modelo de Objeto de Documento (DOM)—una estructura de árbol. Esto facilita la navegación y manipulación de elementos. Utilizando XPath y selectores CSS, puedes localizar cualquier pieza de información que necesites.

## Ver También
- Gema de Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- Documentación de rexml de Ruby: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Analizador alternativo 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Aprender sobre XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
