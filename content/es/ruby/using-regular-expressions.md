---
title:                "Utilizando expresiones regulares"
html_title:           "Ruby: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Usar expresiones regulares en programación consiste en buscar y manipular patrones de texto dentro de una cadena de caracteres. Los programadores recurren a esta técnica para ahorrar tiempo y esfuerzo al procesar grandes cantidades de datos y simplificar la lógica de sus programas.

## Cómo:
Las expresiones regulares en Ruby se definen entre dos barras / / y utilizan diferentes caracteres especiales para representar patrones. Por ejemplo, /hello/ buscará la palabra "hello" en una cadena de caracteres. Para contar cuántas coincidencias hay en una cadena, se puede utilizar el método scan() combinado con la expresión regular. Por ejemplo:
```Ruby 
   "hello, goodbye, hello".scan(/hello/)
   #=> ["hello", "hello"]
```

## Inmersión Profunda:
Las expresiones regulares tienen su origen en la década de 1950, cuando el matemático Stephen Kleene desarrolló los "autómatas finitos", una herramienta teórica para emparejar patrones de texto. Aunque hay alternativas a las expresiones regulares, como el uso de funciones de cadena más específicas para ciertos patrones, las expresiones regulares siguen siendo una herramienta poderosa y ampliamente utilizada en la programación. En Ruby, se implementan utilizando la biblioteca Oniguruma, que proporciona soporte para varios idiomas y estándares de expresiones regulares.

## Ver También:
Para un tutorial más detallado sobre el uso de expresiones regulares en Ruby, puedes consultar la documentación oficial de Ruby en https://ruby-doc.org/core-2.7.0/Regexp.html. También puedes explorar otras herramientas y bibliotecas relacionadas en la página de RubyGems dedicada a expresiones regulares: https://rubygems.org/search?utf8=%E2%9C%93&query=regular+expressions.