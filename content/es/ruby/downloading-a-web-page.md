---
title:                "Descargando una página web"
html_title:           "Ruby: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Descargar una página web es cuando un programador obtiene el código fuente de una página web por medio de un lenguaje de programación. Los programadores lo hacen para poder acceder y manipular la información de la página web, ya sea para obtener datos específicos o para automatizar tareas.

## Cómo:

```ruby
require 'open-uri'
# Requerir el módulo "open-uri" para poder usar los métodos de descarga de páginas web

webpage = URI.open("https://example.com")
# Crear una variable que almacene la página web a descargar

puts webpage.read
# Imprimir el contenido de la página web, obtenido a través del método .read

# Output:
# <!doctype html>
# <html>
# ...
```

## Profundizando:

La descarga de páginas web ha sido una técnica común en la programación desde los primeros años de Internet. Existen diversos métodos para hacerlo, como utilizar herramientas de línea de comandos como cURL o wget, o bien implementar código en un lenguaje de programación como Ruby. También se pueden descargar páginas web completas con su estructura y recursos por medio de herramientas como HTTrack.

## Vea también:

- [La documentación oficial de open-uri en Ruby](https://ruby-doc.org/stdlib-2.7.2/libdoc/open-uri/rdoc/URI.html)
- [La guía de cURL para descargar páginas web](https://curl.se/docs/manual.html)
- [El sitio web de HTTrack para descargar páginas web completas](https://www.httrack.com/)