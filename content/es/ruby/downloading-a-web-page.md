---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Descargar una página web implica obtener su código fuente HTML para uso y análisis local. Los programadores lo hacen para scrapping de datos, automatización de pruebas, entre otras tareas.

## ¿Cómo hacerlo?

Aquí se muestra cómo descargar una página web con el módulo `open-uri` y Nokogiri. 

```Ruby
require 'open-uri'
require 'nokogiri'

# Descargando el código HTML 
html_content = open('https://www.example.com').read

# Parseando el HTML con Nokogiri
nokogiri_object = Nokogiri::HTML(html_content)

puts nokogiri_object
```

La salida será el contenido HTML del sitio web que especificaste.

## Análisis detallado

Históricamente, la descarga de páginas web estaba limitada a programas de línea de comandos como `wget` o `curl`. Sin embargo, con la llegada de lenguajes de programación más potentes como Ruby, este proceso se ha vuelto más sencillo y personalizable.

Existen alternativas para descargar páginas web en Ruby, como el uso de Net::HTTP o HTTParty, ambos ofrecen similares funcionalidades.

Al ejecutar nuestro código, `open-uri` hace una solicitud HTTP GET a la URL que proporcionamos, descarga el contenido HTML y lo convierte en un objeto IO. Nokogiri toma el string resultante y lo parsea en un objeto que podemos navegar y jugar como si fuéramos el navegador.

## Ver también

Para obtener información adicional, consulta las siguientes fuentes:

- [Documentación de Ruby OpenSSL](https://ruby.github.io/openssl/)
- [Gem Nokogiri](https://nokogiri.org/)
- [Documentación de Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTParty Gem](https://github.com/jnunemaker/httparty)