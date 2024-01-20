---
title:                "Capitalizando una cadena de texto"
html_title:           "Ruby: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Capitalizar una cadena de texto en programación se refiere a convertir su primera letra a mayúscula. Los programadores lo hacen para mejorar la legibilidad y el formato de la salida.

## Cómo hacerlo:

Aquí hay un ejemplo en Ruby de cómo capitalizar una cadena:

```Ruby
string = "¡hola mundo!"
capitalized_string = string.capitalize
puts capitalized_string
```

Salida esperada:
```Ruby
"¡Hola mundo!"
```

## Profundizamos

Historiamente, las cadenas han sido capitalizadas por necesidades de formato y presentación visual. Sin embargo, en Ruby, hay alternativas a la función .capitalize. Por ejemplo, puedes usar .upcase para convertir toda la cadena en mayúsculas.

Los detalles de implementación para capitalizar una cadena en Ruby son bastante sencillos. La función .capitalize es uno de los muchos métodos incorporados disponibles para las cadenas. Solo afecta a la primera letra de la cadena y deja el resto tal cual.

```Ruby
string = "¡hola mundo!"
uppercased_string = string.upcase
puts uppercased_string
```

Salida esperada:
```Ruby
"¡HOLA MUNDO!"
```

## Ver También:

1. Documentación oficial de Ruby para el método String.capitalize: https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize
2. Una lista de métodos de String en Ruby: https://ruby-doc.org/core-2.7.0/String.html
3. Más ejemplos y uso de métodos de cadena en Ruby, como .capitalize y .upcase: https://www.rubyguides.com/2018/01/ruby-string-methods/