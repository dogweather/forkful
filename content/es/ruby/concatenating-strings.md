---
title:                "Ruby: Uniendo cadenas de texto"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar strings o cadenas de texto es una habilidad esencial en la programación ya que nos permite combinar diferentes strings para crear una cadena más larga y completa. Esto puede ser útil en situaciones como la generación de mensajes personalizados, la construcción de URLs y la creación de mensajes de error.

## Cómo hacerlo

La concatenación de strings en Ruby es muy sencilla. Simplemente usamos el operador + para unir dos o más cadenas. Veamos un ejemplo:

```Ruby
string1 = "Hola"
string2 = "mundo"
puts string1 + " " + string2
```

La salida de este código sería "Hola mundo". Como se puede ver, podemos incluir espacios entre las cadenas para dar formato a la salida. También podemos utilizar el operador << para concatenar strings. Veamos otro ejemplo:

```Ruby
string1 = "¡Bienvenido a"
string2 = " mi blog!"
puts string1 << string2
```

En este caso, la salida sería "¡Bienvenido a mi blog!". Como se puede observar, el operador << es especialmente útil si queremos añadir una cadena al final de otra sin tener que incluir los espacios de forma manual.

## Profundizando

En Ruby, también podemos utilizar el método concat para concatenar strings. Este método es similar al operador << pero puede ser más útil en ciertas situaciones. También podemos usar el método .concat para concatenar una cadena a sí misma varias veces. Veamos un ejemplo:

```Ruby
string = "Happy "
string.concat("birthday!")
puts string  # Salida: Happy birthday!
```

También podemos utilizar el método *format* para agregar variables a una cadena. Tenga en cuenta que la cadena debe tener un marcador de posición para cada variable que se va a agregar. Por ejemplo:

```Ruby
name = "Maria"
puts "Bienvenidos a mi blog, %s!" % name
```

La salida sería "Bienvenidos a mi blog, Maria!". En este caso, el marcador de posición %s es reemplazado por la variable name.

## Ver también

- [Documentación oficial de Ruby sobre concatenación de strings](https://ruby-doc.org/core-2.7.1/String.html#method-i-2B)
- [Tutorial de concatenación de strings en Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Post en el blog de Ruby sobre concatenación de strings y otros métodos útiles](https://www.dreamincode.net/forums/topic/195829-useful-string-methods/)