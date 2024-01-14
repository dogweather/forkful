---
title:    "Ruby: Convirtiendo una cadena a minúsculas"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## ¿Por qué Convertir una Cadena a Minúsculas en Ruby?

Convertir una cadena de texto a minúsculas es una tarea común en la programación. Puede ser útil para comparar cadenas sin importar las mayúsculas y minúsculas o para formatear la salida del texto. En Ruby, hay varias formas de lograr esto, lo cual veremos en esta publicación.

## Cómo Hacerlo

En Ruby, hay dos métodos principales para convertir una cadena a minúsculas: `downcase` y `capitalize`. Veamos un ejemplo de cada uno:

```ruby
# Método downcase
cadena = "Hola MUNDO"
puts cadena.downcase # salida: hola mundo

# Método capitalize
cadena = "Hola MUNDO"
puts cadena.capitalize # salida: Hola mundo
```

Como puedes ver, `downcase` convierte todas las letras de la cadena a minúsculas, mientras que `capitalize` solo convierte la primera letra a mayúscula y el resto a minúsculas.

También puedes usar el método `swapcase`, que convierte las letras mayúsculas en minúsculas y viceversa:

```ruby
cadena = "HoLa MuNdO"
puts cadena.swapcase # salida: hOlA mUnDo
```

Otra opción es utilizar el método `downcase!` que modifica la cadena original en lugar de crear una nueva:

```ruby
cadena = "Hola MUNDO"
cadena.downcase!
puts cadena # salida: hola mundo
```

## Profundizando

Aunque los métodos mencionados anteriormente son los más comunes, también hay otras formas para convertir una cadena a minúsculas en Ruby. Una de ellas es utilizando la librería `i18n`, que permite convertir cadenas a una variedad de formatos de capitalización, incluyendo el inglés y el español.

Puedes instalar la librería con el siguiente comando:

```
gem install i18n
```

Y luego importarla en tu código:

```ruby
require 'i18n'
```

A continuación, puedes utilizar el método `transliterate` para obtener la versión en minúsculas de una cadena:

```ruby
cadena = "CÓMO CONVERTIR UNA CADENA A MINÚSCULAS EN RUBY"
puts I18n.transliterate(cadena) # salida: cómo convertir una cadena a minúsculas en ruby
```

Otra opción interesante es utilizar expresiones regulares para convertir una cadena a minúsculas. Este enfoque puede ser útil si quieres reemplazar ciertos caracteres con otros, además de convertir a minúsculas. Por ejemplo, si quieres cambiar todos los espacios en blanco por guiones bajos, puedes hacerlo de la siguiente manera:

```ruby
cadena = "Esto es un ejemplo"
puts cadena.downcase.gsub(/\s/, '_') # salida: esto_es_un_ejemplo
```

## Consulta También

- [Documentación oficial de Ruby sobre métodos de cadena](https://ruby-doc.org/core-2.7.1/String.html)
- [Ejemplo de uso de i18n en Ruby](https://www.rubyguides.com/2018/07/ruby-i18n-gem/)
- [Expresiones regulares en Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)