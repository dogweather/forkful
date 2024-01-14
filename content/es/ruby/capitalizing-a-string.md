---
title:    "Ruby: Capitalizando una cadena"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena en Ruby

Capitalizar una cadena en Ruby es una tarea común en la programación. Es útil cuando se desea resaltar una palabra o frase en un texto, o cuando se quiere seguir una convención de estilo específica. A continuación, explicaremos cómo se puede hacer esto en Ruby.

## Cómo capitalizar una cadena en Ruby

```ruby
cadena = "hola, mundo"
puts cadena.capitalize
```

Salida: "Hola, mundo"

En el ejemplo anterior, utilizamos el método `capitalize` para capitalizar la primera letra de la cadena `hola, mundo`. Este método devuelve una copia de la cadena con la primera letra en mayúscula.

Además, también podemos utilizar el método `capitalize!` para modificar la cadena original en lugar de crear una nueva:

```ruby
cadena = "hola, mundo"
cadena.capitalize!
puts cadena
```

Salida: "Hola, mundo"

Si la cadena comienza con una letra mayúscula, estos métodos no harán ningún cambio en la cadena original.

## Profundizando en la capitalización de cadenas en Ruby

Existen otros métodos en Ruby que nos permiten capitalizar una cadena de distintas formas. Por ejemplo, el método `upcase` nos permite convertir todas las letras de una cadena en mayúsculas:

```ruby
cadena = "hola, mundo"
puts cadena.upcase
```

Salida: "HOLA, MUNDO"

Mientras que el método `downcase` hace lo contrario, convirtiendo todas las letras en minúsculas:

```ruby
cadena = "HOLA, MUNDO"
puts cadena.downcase
```

Salida: "hola, mundo"

Además, el método `swapcase` nos permite intercambiar las letras mayúsculas y minúsculas en una cadena:

```ruby
cadena = "Hola, Mundo"
puts cadena.swapcase
```

Salida: "hOLA, mUNDO"

Estos métodos pueden ser útiles en diferentes situaciones, dependiendo de nuestras necesidades.

## Ver también

- Documentación oficial de Ruby sobre el método `capitalize`: https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize
- Tabla de contenido de la documentación de Ruby: https://ruby-doc.org/core-2.7.1/