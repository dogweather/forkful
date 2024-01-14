---
title:                "Ruby: Analizando html"
simple_title:         "Analizando html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo los navegadores web pueden mostrar de forma organizada y visualmente atractiva el contenido de una página web? La respuesta está en el lenguaje de marcado HTML. Pero, ¿cómo podemos acceder y utilizar esa información en nuestros programas? ¡La respuesta está en el análisis sintáctico (parsing) de HTML con Ruby!

## Cómo hacerlo

Para analizar sintácticamente (parsear) HTML en Ruby, primero necesitamos instalar una gema (gem) llamada 'Nokogiri'. Luego, podemos utilizar su método `Nokogiri::HTML` para abrir un archivo HTML y convertirlo en un objeto 'Nokogiri::HTML::Document', que contiene todos los elementos del documento HTML. A continuación, podemos utilizar el método `search` en el objeto document para buscar elementos específicos en el documento utilizando selectores CSS o XPath. Por ejemplo:

```Ruby
require 'nokogiri'

# Abrir el archivo HTML y guardar su contenido en una variable
html = File.open("pagina_web.html")

# Convertir el contenido en un objeto 'Nokogiri::HTML::Document'
documento = Nokogiri::HTML(html)

# Utilizar el método 'search' para buscar todos los elementos <h1> en el documento
titulos = documento.search('h1')

# Imprimir el contenido de cada elemento <h1>
titulos.each do |titulo|
  puts titulo.text
end
```

Este es solo un ejemplo básico de cómo podemos utilizar el análisis sintáctico de HTML en Ruby. Conocer selectores CSS y XPath puede ayudarnos a encontrar elementos específicos en un documento HTML de manera más eficiente y precisa.

## Profundizando

Una parte importante del análisis sintáctico de HTML es la comprensión de cómo está estructurado el documento HTML y cómo podemos navegar a través de él. Por ejemplo, podemos acceder a los elementos hijos de un elemento utilizando el método `children`, y podemos obtener los atributos de un elemento utilizando el método `attributes`. También es importante tener en cuenta que hay variaciones en cómo se escribe HTML, por lo que es posible que necesitemos manejar ciertos casos especiales en nuestro código.

Para obtener más información sobre parsing de HTML con Ruby, te recomendamos consultar la documentación de la gema Nokogiri y practicar con diferentes archivos HTML.

## Ver también

- Documentación de la gema Nokogiri: https://www.nokogiri.org/
- Tutorial de análisis sintáctico de HTML con Ruby: https://www.rubyguides.com/2018/10/parsing-html-nokogiri/
- Ejemplos de XPath y selectores CSS: https://code.tutsplus.com/es/tutorials/geeking-out-with-ruby-parsing-html-with-the-nokogiri-gem--cms-20445