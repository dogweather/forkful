---
title:                "Capitalizando una cadena de texto"
date:                  2024-02-03T19:06:07.376032-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando una cadena de texto"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Capitalizar una cadena en programación a menudo se refiere a convertir el primer carácter de una cadena a mayúscula y el resto a minúscula. Los programadores hacen esto por razones como adherirse a convenciones de nombres, hacer que las salidas sean más legibles o asegurar la consistencia de los datos para comparaciones y almacenamiento.

## Cómo:
Ruby proporciona métodos directos para la manipulación de cadenas, incluida la capitalización. Así es cómo puedes capitalizar una cadena en Ruby:

```ruby
# Método incorporado de Ruby
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

El método `.capitalize` de Ruby es conveniente pero solo afecta a la primera letra. Para tener más control o para capitalizar cada palabra en una cadena (conocido como caso de título), podrías querer usar el método `titleize` de la extensión ActiveSupport de Rails, o implementarlo tú mismo:

```ruby
# Usando 'titleize' de ActiveSupport en Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Si no estás utilizando Rails o prefieres una solución puramente en Ruby, así es cómo podrías capitalizar cada palabra en una cadena:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Este método divide la cadena en un arreglo de palabras, capitaliza cada una, luego las une nuevamente con un espacio.
