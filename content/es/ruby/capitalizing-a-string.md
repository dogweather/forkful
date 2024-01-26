---
title:                "Capitalizando una cadena de texto"
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Capitalizar una cadena en Ruby significa convertir la primera letra de cada palabra a mayúscula. Los programadores hacen esto para estandarizar datos, mejorar la legibilidad o cumplir con requisitos de formato de texto en aplicaciones.

## Cómo hacerlo:

Ruby facilita capitalizar cadenas con el método `capitalize`. Pero si quieres capitalizar todas las palabras en una cadena, tendrás que dividir la cadena en palabras y capitalizar cada una. 

```Ruby
# Capitaliza solo la primera palabra de una cadena
puts "hola mundo".capitalize    #=> "Hola mundo"

# Capitaliza todas las palabras de una cadena
puts "hola mundo".split.map(&:capitalize).join(' ')   #=> "Hola Mundo"

# O usando el método titleize de ActiveSupport (Rails)
require 'active_support/core_ext/string/inflections'
puts "hola mundo".titleize      #=> "Hola Mundo"
```

## Profundizando:

Antes de que Ruby ofreciera métodos convenientes para capitalizar, los programadores tenían que manipular cadenas manualmente, lo cual era propenso a errores y no muy eficiente. Además de `capitalize`, ahora también tenemos métodos como `upcase`, `downcase`, y `swapcase`, útiles para diferentes necesidades.

La capitalización automática puede no ser perfecta para todos los idiomas o casos (por ejemplo, no maneja bien las abreviaturas o acrónimos). En entornos como Rails, ActiveSupport agrega el método `titleize`, que es más completo y configurado para lidiar con los casos más comunes en titulación de palabras.

Es importante mencionar que la capitalización es dependiente del contexto y de las reglas de cada idioma. Ruby maneja bastante bien la mayoría de las reglas de capitalización en inglés, pero para otros idiomas pueden ser necesarias soluciones personalizadas.

## Ver También:

Para más información, puedes consultar la documentación oficial de Ruby sobre los métodos de [String](https://ruby-doc.org/core-2.7.0/String.html), así como la [guía de ActiveSupport sobre inflections](https://api.rubyonrails.org/classes/ActiveSupport/Inflector.html). Si estás interesado en el manejo más avanzado de cadenas y capitalización en diferentes idiomas, podrías explorar gemas como [Rails i18n](https://github.com/svenfuchs/rails-i18n).
