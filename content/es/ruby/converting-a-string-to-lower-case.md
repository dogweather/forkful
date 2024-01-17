---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Ruby: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# ¿¿Qué y por qué?####
Convertir una cadena de texto a minúsculas es un proceso común en la programación que consiste en cambiar todas las letras de una cadena a su forma minúscula. Esto se realiza para estandarizar el texto y facilitar su manipulación, ya que las letras mayúsculas y minúsculas pueden afectar el resultado de una búsqueda o comparación de cadenas.

# ¿Como?####
```Ruby
cadena = "Hola Mundo"
puts cadena.downcase
```
```
=> hola mundo
```

# Inmersión Profunda####
Algunos lenguajes de programación como C y Java requieren que se utilicen funciones específicas para convertir una cadena de texto a minúsculas, mientras que en Ruby, se puede lograr simplemente utilizando el método `.downcase` en una cadena de texto. También existen alternativas al método `.downcase`, como `.swapcase` que cambia entre mayúsculas y minúsculas y `.capitalize` que transforma la primera letra de la cadena a mayúscula.

# Ver también####
- [Documentación de Ruby sobre el método downcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Ejemplos de uso del método downcase en situaciones reales](https://www.geeksforgeeks.org/ruby-string-downcase-method/)