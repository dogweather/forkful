---
title:    "Ruby: Eliminando caracteres que coinciden con un patrón"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, cuando escribimos código en Ruby, nos encontramos con la necesidad de eliminar caracteres que coinciden con un cierto patrón. Esto puede ser especialmente útil cuando estamos limpiando datos o trabajando con cadenas de texto que contienen información inútil.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Ruby, podemos utilizar el método `gsub` junto con una expresión regular. Veamos un ejemplo:

```Ruby
string = "¡Hola! ¿Cómo estás?"
string.gsub(/[!¡¿?]/, "")
```

En este ejemplo, estamos utilizando un patrón de expresión regular que coincide con todos los signos de puntuación en la cadena `string`. Luego, usamos el método `gsub` para reemplazar todas las coincidencias con una cadena vacía. Esto nos devuelve la cadena `Hola Cómo estás`.

También podemos utilizar el método `gsub` para eliminar patrones específicos de la cadena. Por ejemplo, si queremos eliminar todas las letras mayúsculas de una cadena, podemos hacer lo siguiente:

```Ruby
string = "Hola RUBY"
string.gsub(/[A-Z]/, "")
```

En este caso, estamos eliminando todos los caracteres de la cadena que coinciden con el patrón de expresión regular `[A-Z]`, lo que nos devuelve `ola`.

## Profundizar en el tema

Para aquellos que quieran profundizar en la eliminación de caracteres que coinciden con un patrón en Ruby, hay algunas cosas a tener en cuenta:

- Las expresiones regulares pueden ser complicadas de entender al principio, pero una vez que entiendes cómo funcionan, pueden ser muy poderosas para manipular texto.
- Además de `gsub`, también existen otros métodos en Ruby que nos permiten eliminar caracteres que coinciden con un patrón, como `sub` y `delete`.
- Podemos utilizar el método `scan` para encontrar todas las ocurrencias de un patrón en una cadena y luego usar `gsub` para eliminarlas.

Con un buen conocimiento de expresiones regulares y los métodos de manipulación de cadenas en Ruby, podemos hacer mucho más que simplemente eliminar caracteres que coinciden con un patrón. Podemos realizar cambios complejos en las cadenas y extraer información valiosa de ellas.

## Ver también

- [Documentación de Ruby sobre expresiones regulares](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Documentación de Ruby sobre el método `gsub`](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Guía de expresiones regulares en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)