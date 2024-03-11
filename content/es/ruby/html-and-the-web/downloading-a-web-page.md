---
date: 2024-01-20 17:44:57.074608-07:00
description: "Descargar una p\xE1gina web significa traer su contenido a tu equipo\
  \ local. Programadores hacen esto para analizar la informaci\xF3n, probar la conectividad\
  \ o\u2026"
lastmod: '2024-03-11T00:14:33.431250-06:00'
model: gpt-4-1106-preview
summary: "Descargar una p\xE1gina web significa traer su contenido a tu equipo local.\
  \ Programadores hacen esto para analizar la informaci\xF3n, probar la conectividad\
  \ o\u2026"
title: "Descargando una p\xE1gina web"
---

{{< edit_this_page >}}

## Qué es y por qué?
Descargar una página web significa traer su contenido a tu equipo local. Programadores hacen esto para analizar la información, probar la conectividad o monitorear cambios en el sitio.

## Cómo hacerlo:
Ruby hace esto fácil con bibliotecas como `open-uri` y `nokogiri`. Aquí hay un ejemplo básico con `open-uri`:

```ruby
require 'open-uri'

# Abre la URL y lee el contenido
contenido = open('https://www.ejemplo.com').read

puts contenido
```

Salida muestra el HTML de `www.ejemplo.com`.

Si quieres algo más avanzado, por ejemplo, parsear el HTML, usa `nokogiri`:

```ruby
require 'open-uri'
require 'nokogiri'

# Descargar la página
pagina = Nokogiri::HTML(URI.open('https://www.ejemplo.com'))

# Buscar elementos específicos
titulos = pagina.css('h1')

# Imprimir los títulos
titulos.each { |titulo| puts titulo.content }
```

Esto imprimirá todos los títulos `<h1>` de `www.ejemplo.com`.

## Profundizando:
Descargar web empezó con comandos como `wget` o `curl`. Con el tiempo, lenguajes de programación ofrecieron estas funciones directamente. En Ruby, antes de `open-uri`, se usaba `net/http`, que es más complejo pero poderoso.

Alternativas a `open-uri` y `nokogiri` incluyen `Mechanize`, que actúa más como un navegador, almacenando cookies y siguiendo redirecciones automáticamente.

En cuanto a detalle de implementación, al usar `open-uri`, ten en cuenta que maneja automáticamente las redirecciones y siempre deberías manejar excepciones para los posibles errores de conexión.

## Ver También:
- [RubyDoc open-uri](https://rubydoc.info/stdlib/open-uri)
- [Nokogiri Documentación](https://nokogiri.org)
- [Mechanize GitHub](https://github.com/sparklemotion/mechanize)
- [Ruby Net::HTTP Documentación](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
