---
title:                "Ruby: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador de Ruby, probablemente hayas escuchado sobre las expresiones regulares. Estas son secuencias de caracteres que permiten realizar búsquedas y manipulaciones en cadenas de texto de una manera muy poderosa. Aunque puede parecer intimidante al principio, dominar las expresiones regulares puede ahorrarte una cantidad significativa de tiempo y energía en tus proyectos.

## Cómo hacerlo

Para utilizar expresiones regulares en Ruby, debes seguir los siguientes pasos:

1. Importa el módulo Regexp usando `require 'regexp'`
2. Define tu expresión regular usando la sintaxis `/expresión/`
3. Aplica la expresión regular usando uno de los métodos de cadena de Ruby, como `match()` o `scan()`

Veamos un ejemplo práctico. Digamos que tienes una lista de correos electrónicos y quieres filtrar solo los que terminan en ".com". Utilizando expresiones regulares, puedes hacerlo de la siguiente manera:

```Ruby
require 'regexp'
emails = ["user1@domain.com", "user2@domain.net", "user3@domain.com"]
emails.each do |email|
  if email.match(/\.com$/) # $ representa el final de la cadena
    puts email
  end
end
```

Este código imprimirá los dos primeros correos electrónicos, ya que solo ellos cumplen con la expresión regular.

## Inmersión profunda

Ahora que ya sabes cómo utilizar expresiones regulares básicas en Ruby, es importante profundizar un poco más para manejar casos más complejos. Algunos conceptos que debes tener en cuenta incluyen:

- Caracteres especiales: hay ciertos caracteres, como `*` o `+`, que tienen un significado especial en expresiones regulares y deben escaparse con `\` para que se interpreten literalmente.
- Clases de caracteres: puedes utilizar `[ ]` para especificar un conjunto de caracteres. Por ejemplo, `[aeiou]` coincidirá con cualquier vocal.
- Grupos y capturas: puedes agrupar ciertos patrones utilizando `()` y luego acceder a ellos con `$1`, `$2`, etc.
- Modificadores: puedes utilizar varios modificadores al final de tu expresión regular para hacerla insensible a mayúsculas y minúsculas, entre otras cosas.

Para obtener más información sobre estos conceptos y otros trucos útiles, te recomendamos revisar la documentación oficial de Ruby sobre expresiones regulares en [este enlace](https://ruby-doc.org/core-2.7.2/Regexp.html).

## Ver también

- [Tutorial básico sobre expresiones regulares en Ruby](https://medium.com/@wizardofmath/regex-for-all-827806351791)
- [Documentación oficial sobre expresiones regulares en Ruby](https://ruby-doc.org/core-2.7.2/Regexp.html)
- [Expresiones regulares en Ruby: consejos y trucos](https://blog.appsignal.com/2018/08/21/regex-on-ruby-the-good-the-bad-and-the-ugly.html)