---
title:                "Ruby: Eliminando caracteres que coinciden con un patrón."
simple_title:         "Eliminando caracteres que coinciden con un patrón."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, en la programación en Ruby, es necesario eliminar ciertos caracteres de una cadena de texto que coincidan con cierto patrón. Esto puede ser útil para limpiar y formatear datos o para hacer búsquedas más precisas. En esta publicación, exploraremos cómo eliminar caracteres que coincidan con un patrón en Ruby.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón en Ruby, utilizaremos el método `gsub` (sustitución global) de la clase `String`. Este método reemplaza todas las apariciones del patrón especificado con una cadena vacía.

Para empezar, debemos crear una variable que contenga la cadena de texto en la que queremos eliminar los caracteres. Por ejemplo, podemos tener una cadena con una serie de nombres separados por comas, y queremos eliminar todas las comas. Dentro de los bloques de código Ruby, usaremos comillas simples para delimitar las cadenas, ya que el método `gsub` no funciona con comillas dobles.

```Ruby
nombres = 'Juan, María, Pedro, Ana'
```

Ahora, podemos usar `gsub` para reemplazar todas las comas con una cadena vacía:

```Ruby
nombres_sin_comas = nombres.gsub(',', '')
puts nombres_sin_comas #=> Juan María Pedro Ana
```

También podemos usar una expresión regular como patrón para eliminar caracteres que coincidan con un patrón en particular. Por ejemplo, si queremos eliminar todas las letras mayúsculas de una cadena, podemos usar la expresión regular `/[A-Z]/`, que coincide con cualquier letra mayúscula:

```Ruby
cadena = 'Este es un ejemplo en Ruby'
cadena_modificada = cadena.gsub(/[A-Z]/, '')
puts cadena_modificada #=> ste s un ejemplo en by
```

## En profundidad

El método `gsub` también acepta un bloque como argumento opcional. Esto significa que en lugar de reemplazar los caracteres con una cadena vacía, podemos personalizar la lógica y manipular los datos más a fondo.

Por ejemplo, si queremos eliminar solo aquellas letras que se encuentren entre paréntesis, podemos usar una expresión regular y un bloque para realizar la eliminación:

```Ruby
cadena = 'Los (paréntesis) deben desaparecer'
cadena_modificada = cadena.gsub(/\(.*?\)/, '') do |match|
  match.gsub(/[a-z]/, '')
end
puts cadena_modificada #=> Los deben desaparecer
```

En este ejemplo, la expresión regular busca cualquier texto entre paréntesis y pasa ese texto al bloque como un parámetro `match`. Dentro del bloque, usamos `gsub` una vez más para eliminar cualquier letra minúscula del texto entre paréntesis. Como resultado, solo eliminamos las letras y las dejamos vacías en su lugar.

## Ver también

- [Expresiones regulares en Ruby](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [La clase `String` en Ruby](https://ruby-doc.org/core-3.0.0/String.html)
- [Método `gsub` en Ruby](https://ruby-doc.org/core-3.0.0/String.html#method-i-gsub)