---
title:                "Capitalizando una cadena"
html_title:           "Ruby: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena?

A veces, es importante capitalizar una cadena de texto para que se vea correctamente en una presentación o para cumplir con un determinado formato. Ruby tiene una función incorporada que facilita la capitalización de cadenas, lo que puede ahorrar tiempo y esfuerzo a los programadores.

## Cómo hacerlo

Usando el método "capitalize" en una cadena, podemos capitalizar la primera letra de esa cadena. Aquí hay un ejemplo:

```Ruby
cadena = "hola mundo"
puts cadena.capitalize

# Output: "Hola mundo"
```

También podemos capitalizar todas las letras de una cadena usando el método "upcase":

```Ruby
otra_cadena = "este es otro ejemplo"
puts otra_cadena.upcase

# Output: "ESTE ES OTRO EJEMPLO"
```

## Profundizando en la capitalización de cadenas

El método "capitalize" solo capitaliza la primera letra de una cadena. Sin embargo, podemos usar otros métodos como "split" y "join" para capitalizar cada palabra en una cadena. Aquí hay un ejemplo:

```Ruby
def capitalize_words(string)
    words_array = string.split(" ")
    words_array.map! { |word| word.capitalize }
    capitalized_string = words_array.join(" ")
    puts capitalized_string
end

capitalize_words("este es un ejemplo de capitalización de palabras")

# Output: "Este Es Un Ejemplo De Capitalización De Palabras"
```

## Ver también

- [Métodos de cadenas en Ruby](https://www.rubyguides.com/2016/04/ruby-string-methods/)
- [Documentación oficial de Ruby sobre el método "capitalize"](https://ruby-doc.org/core-3.0.0/String.html#method-i-capitalize)