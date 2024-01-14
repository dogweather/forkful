---
title:                "Ruby: Buscar y reemplazar texto"
simple_title:         "Buscar y reemplazar texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

La búsqueda y reemplazo de texto es una habilidad importante para cualquier programador de Ruby. Permite realizar cambios rápidos y eficientes en grandes cantidades de texto, lo que ahorra tiempo y evita errores manuales.

## Cómo hacerlo

Para buscar y reemplazar texto en Ruby, utilizaremos el método `gsub()` que toma dos argumentos: el texto que se desea buscar y el texto con el que se desea reemplazarlo. Por ejemplo, si queremos reemplazar todas las letras "a" con la letra "e" en una palabra, podemos hacerlo de la siguiente manera:

```Ruby
texto = "Hola"
texto.gsub("a", "e")  #Output: "Hole"
```

También podemos utilizar expresiones regulares para realizar patrones de búsqueda más complejos. Por ejemplo, si queremos eliminar todos los espacios en blanco en una cadena, podemos hacerlo de la siguiente manera:

```Ruby
texto = "Hola, cómo estás?"
texto.gsub(/\s+/, "") #Output: "Hola,cómoestás?"
```

También es posible utilizar `gsub()` en objetos de tipo `String` y `Array`, lo que lo hace muy versátil y útil en diversas situaciones.

## Profundizando

El método `gsub()` es una abreviación de "Global Substitution", lo que significa que reemplaza todas las coincidencias de una cadena de texto, no solo la primera que encuentre. Además, también existe el método `sub()` que sólo reemplaza la primera coincidencia.

Otra característica útil de `gsub()` es que puede tomar un bloque de código como argumento. Esto significa que se pueden realizar operaciones más complejas en cada coincidencia encontrada. Por ejemplo, si queremos invertir cada palabra en una frase, podemos hacerlo de la siguiente manera:

```Ruby
frase = "Hola, me llamo Juan"
frase.gsub(/\w+/) { |palabra| palabra.reverse } #Output: "aloh, em omall luiJ"
```

## Ver también

- [Documentación de Ruby para ``gsub``](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Expresiones regulares en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ruby en español](https://ruby.org.es/)