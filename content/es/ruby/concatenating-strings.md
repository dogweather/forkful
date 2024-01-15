---
title:                "Uniendo cadenas"
html_title:           "Ruby: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Si estás aprendiendo a programar en Ruby, una de las cosas que seguramente necesitarás hacer es concatenar strings. Esto significa unir dos o más strings para formar uno solo. Puede ser útil en diferentes situaciones, por ejemplo, imprimir un mensaje personalizado o combinar información de diferentes variables.

## Cómo hacerlo

Para concatenar strings en Ruby, puedes utilizar el operador de suma (+) o el método `.concat()`. Veamos algunos ejemplos:

```Ruby
# Utilizando el operador de suma
nombre = "María"
apellido = "García"
edad = 25

mensaje = nombre + " " + apellido + " tiene " + edad.to_s + " años."
puts mensaje # output: María García tiene 25 años.

# Utilizando el método .concat()
nombre = "Juan"
apellido = "Martínez"
edad = 30

mensaje = nombre.concat(" ").concat(apellido).concat(" tiene ").concat(edad.to_s).concat(" años.")
puts mensaje # output: Juan Martínez tiene 30 años.
```

Podemos notar que en ambos casos tuvimos que convertir la variable `edad` a string utilizando el método `.to_s`, ya que no podemos sumar una string con un integer.

## Profundizando

Además de los métodos mencionados anteriormente, existen otras formas de concatenar strings en Ruby. Por ejemplo, el método `.prepend()` que agrega un string al inicio de otro, o el método `.<<()` que permite agregar un string al final de otro. También podemos utilizar el método `.concat()` con múltiples parámetros para unir más de dos strings.

Otra forma de concatenar strings es mediante la interpolación, donde utilizamos el símbolo `#{}` dentro de una string para llamar a una variable o una expresión. Por ejemplo:

```Ruby
nombre = "Laura"
edad = 35

mensaje = "#{nombre} tiene #{edad} años."
puts mensaje # output: Laura tiene 35 años.
```

Finalmente, es importante tener en cuenta que podemos concatenar diferentes tipos de datos, no solo strings. Podemos unir strings con integers, floats, arrays e incluso otros objetos que se puedan convertir a string.

## Ver también

- [Ruby Guide (en español)](https://ruby-doc.org/core-2.7.1/doc/guides/)
- [Documentación oficial de Ruby (en inglés)](https://ruby-lang.org/en/documentation/)