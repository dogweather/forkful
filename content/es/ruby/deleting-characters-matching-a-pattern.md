---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Eliminación de Caracteres que Coinciden con un Patrón en Ruby

## ¿Qué & Por Qué?

La eliminación de caracteres que coinciden con un patrón es una operación que retira ciertos caracteres en una cadena de texto, basándose en un patrón predefinido. Los programadores lo hacen para limpiar o manipular datos de una manera rápida y eficaz.

## Cómo hacerlo

Aquí te mostramos cómo eliminar caracteres que coinciden con un patrón en Ruby:

```Ruby
cadena = "Hola, ¿cómo estás?"
cadena = cadena.delete(",?")  # Elimina comas y signos de interrogación.
puts cadena # Muestra: "Hola cómo estás"
```

El método `.delete` de Ruby nos permite eliminar cualquier caracter que coincida con los caracteres pasados como un string.

## En Profundidad

La función de eliminar caracteres que coinciden con un patrón ha sido parte de los lenguajes de programación durante mucho tiempo, facilitando la manipulación de cadenas de caracteres al eliminar la necesidad de operaciones manuales lentas y propensas a errores.

Existen alternativas a `.delete` en Ruby, como `.gsub`, que puede reemplazar caracteres en lugar de solo eliminarlos. 

En términos de implementación, el método `.delete` opera escaneando la cadena y creando una nueva cadena con los caracteres que no se eliminan. Esto significa que es bastante eficiente en términos de tiempo, pero puede consumir más memoria porque crea una nueva cadena.

## Ver también

Para obtener más información sobre el trabajo con cadenas en Ruby, te recomiendo consultar los siguientes recursos:

- El método `.delete` en la [documentación de Ruby](https://ruby-doc.org/core-2.7.0/String.html#method-i-delete)
- Tutorial sobre cadenas en [RubyMonk]( https://rubymonk.com/learning/books/1-ruby-primer/chapters/5-strings/lessons/31-string-basics)
- Ejemplos de uso del método `.gsub` en [Stack Overflow](https://stackoverflow.com/questions/19445003/using-ruby-gsub-to-replace-a-string-with-variable-content)

La programación es un continuo aprendizaje. ¡No dejes de explorar nuevas formas de resolver problemas!