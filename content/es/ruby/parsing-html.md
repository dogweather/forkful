---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
El análisis sintáctico de HTML, o parsing, es el proceso de analizar el código HTML para entender su estructura y significado. Los programadores lo hacen para manipular, extraer o incluso transformar información en páginas web.

## Cómo hacerlo:
```Ruby
require 'nokogiri'
require 'open-uri'

# HTML de la página web
html = open("https://www.ejemplo.com")

# Usando Nokogiri para analizar el HTML
doc = Nokogiri::HTML(html)

# Extraer títulos de las noticias
doc.css('.caja-titulo').each do |titulo|
  puts titulo.text
end
```

Este script abrirá la página web, analizará el HTML y extraerá el texto de cada elemento con la clase "caja-titulo". Si cambias la URL y la clase CSS, podrías usar este script en cualquier sitio.

## Profundizando
El análisis sintáctico de HTML ha estado presente desde los primeros días de la web para permitir el procesamiento de sitios web de manera programática. Aunque hemos usado Nokogiri, existen muchas otras librerías y lenguajes que pueden hacer eso, cada uno con sus propias ventajas y desventajas.

Además, los detalles de implementación pueden variar. Por ejemplo, algunos analizadores pueden tomar una cadena HTML y convertirla en un árbol de nodos HTML, mientras que otros pueden generar una versión plana, lista de tokens. Ambos enfoques tienen sus usos dependiendo de lo que necesitas hacer con el HTML analizado.

## Ver También
1. [Nokogiri Documentation](https://nokogiri.org/tutorials/parsing_an_html_xml_document.html) - Documentación oficial de Nokogiri.
2. [Open-uri Documentation](https://ruby-doc.org/stdlib-2.6.1/libdoc/open-uri/rdoc/OpenURI.html) - Documentación oficial de la biblioteca 'open-uri' en Ruby.
3. [W3Schools - HTML DOM parsing Guide](https://www.w3schools.com/xml/dom_nodes.asp) - Una guía en profundidad sobre análisis sintáctico de DOM HTML.