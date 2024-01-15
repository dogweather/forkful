---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Ruby: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Converting a string to lower case is a common task in programming, as it allows for easier comparison and manipulation of text. In Ruby, this can be done easily using built-in methods, making it a useful skill to have in your coding arsenal.

## Cómo hacerlo

```Ruby
# Define a string
texto = "¡Hola, Mundo!"

# Convert to lower case using downcase method
puts texto.downcase # Output: ¡hola, mundo!

# You can also use downcase! method to modify the original string
texto.downcase!
puts texto # Output: ¡hola, mundo!
```

La función `downcase` convierte todos los caracteres en la cadena a minúsculas, mientras que `downcase!` cambia directamente la cadena original. También se pueden encadenar métodos para realizar múltiples operaciones en una sola línea, como por ejemplo `texto.downcase.gsub("h", "j")` para cambiar todas las "h" en la cadena a "j" en minúsculas.

## Profundizando

En Ruby, el método `downcase` utiliza el estándar de Unicode para convertir los caracteres a minúsculas. Esto significa que tanto letras como símbolos especiales en diferentes idiomas pueden ser convertidos correctamente.

También hay otros métodos útiles para manipular cadenas en Ruby, como `upcase` para convertir a mayúsculas, `capitalize` para convertir la primera letra a mayúscula, y `swapcase` para intercambiar entre minúsculas y mayúsculas.

## Ver también

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby String Methods](https://www.rubyguides.com/2015/05/ruby-string-methods/)

¡Ahora estás listo para manipular cadenas de texto en Ruby! Empieza a jugar con estas funciones y descubre cómo pueden simplificar tu código. Happy coding!