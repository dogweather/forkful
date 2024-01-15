---
title:                "Encontrando la longitud de una cadena."
html_title:           "Ruby: Encontrando la longitud de una cadena."
simple_title:         "Encontrando la longitud de una cadena."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué 

En la programación, a menudo nos encontramos con situaciones en las que necesitamos saber la longitud de una cadena de texto. Esto puede ser útil para validar la entrada del usuario, manipular y formatear texto, entre otros casos.

## Cómo 

Para encontrar la longitud de una cadena en Ruby, podemos utilizar el método `length` o `size`. Veamos un ejemplo de cómo utilizarlos: 

```Ruby
# Creamos una cadena de texto
text = "Hola amigos"

# Utilizamos el método 'length'
puts text.length # Output: 11

# Utilizamos el método 'size'
puts text.size # Output: 11
```

En este ejemplo, hemos creado una variable `text` que contiene la cadena de texto "Hola amigos". Luego, utilizamos el método `length` y `size` para encontrar la longitud de la cadena, que es de 11 caracteres. Ambos métodos nos darán el mismo resultado, por lo que podemos utilizar cualquiera de ellos según nuestras preferencias.

## Deep Dive

En Ruby, el método `length` y `size` funcionan de la misma manera ya que `size` es simplemente un alias de `length`. Ambos métodos recorren la cadena y cuentan la cantidad de caracteres, incluyendo espacios, símbolos y caracteres especiales. 

Además, estos métodos son útiles para validar la entrada del usuario en un formulario. Por ejemplo, podemos verificar si un campo de texto tiene una longitud mínima o máxima antes de procesarlo.

## Ver también

- [Ruby Strings](https://www.ruby-lang.org/en/documentation/quickstart/2/)
- [Ruby String length Method](https://ruby-doc.org/core-2.5.0/String.html#method-i-length)