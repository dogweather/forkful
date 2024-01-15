---
title:                "Buscando y reemplazando texto"
html_title:           "Ruby: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué buscar y reemplazar texto en Ruby?

Buscar y reemplazar texto es una tarea común en la programación, especialmente al trabajar con cadenas de texto. En Ruby, esto se puede lograr de manera eficiente y efectiva gracias a sus métodos y herramientas dedicadas para la manipulación de strings.

## Cómo hacerlo

Para buscar y reemplazar texto en Ruby, utilizamos el método `gsub` (substitución global) en una cadena de texto. Este método acepta dos argumentos: el patrón a buscar y el reemplazo deseado. Veamos un ejemplo:

```Ruby
texto = "¡Hola, amigos! ¿Cómo están?"
texto.gsub("amigos", "compañeros")  # => ¡Hola, compañeros! ¿Cómo están?
```

En este ejemplo, utilizamos el método `gsub` para reemplazar la palabra "amigos" por "compañeros". El método busca y reemplaza todas las ocurrencias del patrón en la cadena de texto.

También se pueden utilizar expresiones regulares en lugar de una cadena de texto como patrón de búsqueda. Por ejemplo:

```Ruby
texto = "Ruby es un lenguaje de programación increíble"
texto.gsub(/\w+/, "codigo")  # => codigo codigo codigo codigo codigo
```

En este caso, utilizamos una expresión regular para reemplazar cada palabra en la frase por la palabra "codigo".

Otra forma de buscar y reemplazar texto en Ruby es con el método `sub` (substitución). Este método funciona de manera similar a `gsub`, pero solo reemplaza la primera ocurrencia del patrón. Veamos un ejemplo:

```Ruby
texto = "¡Hola, cómo están?"
texto.sub("Hola", "Saludos")  # => ¡Saludos, cómo están?
```

También se pueden utilizar expresiones regulares con el método `sub` para reemplazar la primera ocurrencia de un patrón.

## Profundizando

Ahora que sabemos cómo utilizar `gsub` y `sub` para buscar y reemplazar texto en Ruby, es importante tener en cuenta algunos detalles.

En primer lugar, estos métodos son sensibles a mayúsculas y minúsculas. Esto significa que al buscar un patrón, se deben considerar las mayúsculas y minúsculas. Por ejemplo:

```Ruby
texto = "¡Hola, cómo están?"
texto.gsub("hola", "adiós")  # => ¡Hola, cómo están? (sin cambios)
```

En segundo lugar, estos métodos afectan directamente a la cadena de texto original y la modifican. Si se desea guardar la cadena de texto original y obtener una cadena modificada, se puede utilizar el método `gsub!` o `sub!` respectivamente.

## Ver también

- Documentación oficial de Ruby sobre `gsub`: https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub
- Expresiones regulares en Ruby: https://www.rubyguides.com/2015/06/ruby-regex/
- Uso de mayúsculas y minúsculas en Ruby: https://learnrubythehardway.org/book/ex26.html