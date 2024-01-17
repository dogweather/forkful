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

## ¿Qué y por qué?
Concatenar cadenas (strings) en programación se refiere a combinar o unir dos o más cadenas de texto en una sola. Los programadores lo hacen para crear mensajes o completar tareas que requieren diferentes partes de texto.

## Cómo hacerlo:
Puedes concatenar strings en Ruby usando el operador `+`, como en el siguiente ejemplo:

```Ruby
"¡Hola" + " mundo!"
```

Esto producirá el resultado: `¡Hola mundo!`

También puedes usar el método `concat`, como en el siguiente ejemplo:

```Ruby
"¡Hola".concat(" mundo!")
```

La salida será la misma: `¡Hola mundo!`

## Deep Dive:
En el pasado, los lenguajes de programación utilizaban métodos más complicados para concatenar cadenas, como asignar una cadena al final de otra. Sin embargo, con las constantes mejoras en los lenguajes de programación, como Ruby, ahora es más sencillo y eficiente utilizar operadores o métodos específicos para concatenar.

Otra alternativa para concatenar strings en Ruby es utilizando el método `<<`, como en el siguiente ejemplo:

```Ruby
"¡Hola" << " mundo!"
```

Y la salida sería: `¡Hola mundo!`

Este método es especialmente útil si quieres añadir texto a una variable existente en lugar de crear una nueva cadena.

## Ver también:
Más información sobre Ruby y concatenación de cadenas: [Ruby Strings](https://ruby-doc.org/core-2.7.0/String.html)