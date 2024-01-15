---
title:                "Extrayendo subcadenas"
html_title:           "Ruby: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en Ruby?

Extraer subcadenas en Ruby puede ser útil cuando se trabaja con cadenas de texto largas y se necesita acceder a una parte específica de la cadena. Por ejemplo, si tienes una dirección que incluye el nombre de la calle, ciudad, estado y código postal, puedes extraer solo el código postal para utilizarlo en otro lugar de tu código.

## Cómo hacerlo

Para extraer una subcadena en Ruby, utilizamos el método `[]` en una cadena de texto seguido de dos números entre corchetes. El primer número representa el índice de inicio y el segundo número representa la longitud de la subcadena a extraer. Por ejemplo:

```Ruby
texto = "¡Hola mundo!"
puts texto[0, 4]
```

Este código imprimirá "¡Hola" ya que inicia en el índice 0 y la longitud es de 4 caracteres.

También podemos utilizar un rango de índices para extraer una subcadena. Por ejemplo:

```Ruby
texto = "¡Hola mundo!"
puts texto[1..3]
```

Este código imprimirá "ola" ya que el rango incluye el índice 1 y va hasta el índice 3, pero no incluye el índice 4.

## Profundizando

Además de utilizar números para especificar el índice de inicio y la longitud de la subcadena, también podemos utilizar métodos de Ruby que nos facilitan la extracción de subcadenas.

Por ejemplo, el método `slice` también se puede utilizar para extraer una subcadena en lugar del método `[]`. También podemos utilizar métodos como `first`, `last` y `drop` para obtener una parte específica de la cadena. Por ejemplo:

```Ruby
texto = "¡Hola mundo!"
puts texto.slice(5..-1)
puts texto.first(4)
puts texto.last(5)
puts texto.drop(6)
```

La salida de este código sería:

```
mundo!
¡Hol
undo!
 mundo!
```

## Ver también

- [Ruby String documentation](https://ruby-doc.org/core-3.0.2/String.html)
- [Ruby Range documentation](https://ruby-doc.org/core-3.0.2/Range.html)
- [Ruby Array documentation](https://ruby-doc.org/core-3.0.2/Array.html)