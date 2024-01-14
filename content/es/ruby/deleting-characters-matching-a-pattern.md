---
title:                "Ruby: Eliminando caracteres que coincidan con un patrón"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué eliminar caracteres que coinciden con un patrón?

Eliminar caracteres que coinciden con un patrón es una técnica comúnmente utilizada en programación para limpiar y manipular cadenas de texto. Esto puede ser útil para eliminar caracteres innecesarios, como espacios en blanco o signos de puntuación, o para extraer información específica de una cadena. Aprender a hacer esto puede ser útil para mejorar la eficiencia y la legibilidad de tu código.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Ruby, puedes utilizar el método `gsub` (abreviación de "global substitution"). Este método busca en una cadena un patrón determinado y lo reemplaza con otro texto especificado.

Supongamos que tenemos la siguiente cadena de texto:

``` Ruby
texto = "Hola, ¡aquí hay una frase con signos de puntuación! ;)"
```

Si queremos eliminar todos los signos de puntuación de esta cadena, podemos hacerlo de la siguiente manera:

``` Ruby
limpio = texto.gsub(/[[:punct:]]/, '')
puts limpio

# Output: Hola aquí hay una frase con signos de puntuación
```

En este ejemplo, utilizamos una expresión regular para buscar cualquier carácter que sea un signo de puntuación y lo reemplazamos con una cadena vacía, lo que efectivamente elimina esos caracteres de la cadena original.

Otro caso común es eliminar espacios en blanco de una cadena. En este caso, podemos utilizar la misma lógica pero cambiando el patrón para buscar cualquier espacio en blanco:

``` Ruby
sin_espacios = texto.gsub(/\s+/, '')
puts sin_espacios

# Output: Hola,aquíhayunafraseconsignosdepuntuación:)
```

## Profundizando

Existen otras formas de eliminar caracteres que coinciden con un patrón en Ruby, como utilizar el método `delete` o `tr`. Sin embargo, `gsub` es el más versátil, ya que permite utilizar expresiones regulares más complejas para encontrar patrones específicos en una cadena.

Es importante tener en cuenta que `gsub` cambiará la cadena original, por lo que si necesitas conservar la cadena original, puedes guardarla en una variable diferente antes de aplicar el método.

## Ver también

- [Documentación de `gsub`](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Tutorial de expresiones regulares en Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)