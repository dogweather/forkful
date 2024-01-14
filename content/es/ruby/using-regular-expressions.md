---
title:                "Ruby: Utilizando expresiones regulares"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar expresiones regulares en Ruby?

Las expresiones regulares son una herramienta útil en la programación de Ruby que permite buscar y manipular patrones dentro de strings. Con su uso, podemos realizar tareas de búsqueda, validación y reemplazo de manera más eficiente y precisa.

## ¿Cómo utilizar expresiones regulares en Ruby?

El uso de expresiones regulares en Ruby es relativamente sencillo y consiste en utilizar una combinación de caracteres especiales y símbolos para crear un patrón a ser buscado dentro de un string.

Un ejemplo básico sería utilizar el método `match` para buscar una coincidencia con una expresión regular en un string específico:

```Ruby
texto = "Hola mundo"
expresion_regular = /mundo/

puts texto.match(expresion_regular)

# Output: mundo
```

También se pueden utilizar los métodos `sub` y `gsub` para reemplazar parte de un string con una expresión regular:

```Ruby
texto = "Hola mundo ¿Cómo estás?"
expresion_regular = /¿Cómo estás/
nuevo_texto = texto.gsub(expresion_regular, "bien")

puts nuevo_texto

# Output: Hola mundo bien
```

Existen muchos más métodos y técnicas avanzadas para utilizar expresiones regulares en Ruby, por lo que se recomienda investigar y experimentar para descubrir todas sus posibilidades.

## Profundizando en el uso de expresiones regulares

Las expresiones regulares en Ruby se pueden utilizar de diferentes maneras, por ejemplo:

- Utilizar el operador `=~` para encontrar una coincidencia en un string.
- Utilizar los delimitadores `/.../` para crear una expresión regular.
- Utilizar el método `scan` para buscar todas las coincidencias de un patrón en un string.
- Utilizar los métodos `split` y `partition` para dividir un string en base a una expresión regular.
- Utilizar los grupos de captura `(...)` para almacenar partes de una coincidencia y utilizarlos en el reemplazo.

Es importante mencionar que las expresiones regulares en Ruby siguen una sintaxis específica, por lo que es recomendable revisar la documentación oficial para conocer todas las posibilidades y detalles.

## Ver también

- [Documentación oficial de expresiones regulares en Ruby](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Tutorial de expresiones regulares en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Expresiones regulares en Ruby: trucos y consejos](https://www.rubyguides.com/2015/06/ruby-regex/)