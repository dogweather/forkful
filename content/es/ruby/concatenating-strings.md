---
title:                "Ruby: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
La concatenación de cadenas es una técnica clave en la programación Ruby para combinar múltiples cadenas en una sola. Esto es útil para crear mensajes personalizados, generar código dinámicamente o para cualquier tarea que requiera la combinación de cadenas.

## Cómo hacerlo
Para concatenar cadenas en Ruby, podemos usar el operador de suma (+) o el método ```.concat```. Veamos algunos ejemplos:

```ruby
# Usando el operador +
nombre = "María"
apellido = "García"
nombre_completo = nombre + " " + apellido
puts nombre_completo  # María García

# Usando el método .concat
mensaje = "¡Hola "
nombre = "Carlos"
mensaje.concat(nombre)
puts mensaje # ¡Hola Carlos
```

## Profundizando
Además de estos métodos básicos, también podemos usar el método ```.prepend``` para agregar texto al principio de una cadena, y el método ```.insert``` para insertar texto en una posición específica. También podemos usar el interpolación de cadenas para combinar variables dentro de una cadena, encerrándolas entre llaves y un signo de dólar ($).

```ruby
# Usando .prepend
nombre = "Ana"
mensaje = "Feliz cumpleaños "
mensaje.prepend(nombre + ", ")
puts mensaje # Ana, Feliz cumpleaños

# Usando .insert
nombre = "Pedro"
saludo = "¡Bienvenido a Ruby!"
mensaje.insert(11, nombre + ", ")
puts mensaje # ¡Bienvenido a Ruby, Pedro!

# Interpolación de cadenas
edad = 25
mensaje = "Tengo #{edad} años de edad"
puts mensaje # Tengo 25 años de edad
```

## Ver también
- Documentación oficial de Ruby sobre la concatenación de cadenas: https://ruby-doc.org/core-2.7.1/String.html#method-i-2B
- Ejemplos prácticos de concatenación de cadenas: https://www.geeksforgeeks.org/how-to-concatenate-two-strings-in-ruby/
- Experiencias y recomendaciones de programadores sobre la concatenación de cadenas en Ruby: https://stackoverflow.com/questions/4582207/how-to-concatenate-strings-in-ruby