---
title:                "Ruby: Capitalización de una cadena"
simple_title:         "Capitalización de una cadena"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar cadenas en Ruby?

Capitalizar una cadena en Ruby es una tarea común y útil al trabajar con strings. Al capitalizar una cadena, podemos cambiar la primera letra de cada palabra a mayúscula, lo que puede mejorar la legibilidad y el formato de nuestros datos.

## Cómo capitalizar una cadena en Ruby

Para capitalizar una cadena en Ruby, podemos utilizar el método `capitalize` en un objeto de tipo string. Por ejemplo, si tenemos la cadena "hola a todos", podemos capitalizarla de la siguiente manera:

```Ruby
"hola a todos".capitalize  # outputs "Hola a todos"
```

También podemos utilizar el método `capitalize!` para modificar la cadena original:

```Ruby
str = "hola a todos"
str.capitalize!  # now the value of str is "Hola a todos"
```

Además, si queremos capitalizar cada palabra en una cadena, podemos usar el método `titleize` del activo de Rails o la gema de terceros `titleize`.

## Profundizando en la capitalización de cadenas

La capitalización de cadenas en Ruby se basa en el método `capitalize` de la clase `String`. Este método toma como argumento una opción booleana `swapcase` que, si se establece en `true`, también cambiará las letras mayúsculas a minúsculas y viceversa. También podemos utilizar el método `capitalize` con una letra mayúscula como argumento para especificar qué letra queremos que sea la primera en la cadena capitalizada.

Un detalle importante a tener en cuenta es que el método `capitalize` solo capitaliza la primera letra de la cadena, mientras que el método `titleize` capitaliza la primera letra de cada palabra.

## Ver también

- Documentación oficial de Ruby sobre el método `capitalize`: https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize
- Documentación oficial de Rails sobre el método `titleize`: https://api.rubyonrails.org/v6.1.3.2/classes/String.html#method-i-titleize
- Gema `titleize` en RubyGems: https://rubygems.org/gems/titleize