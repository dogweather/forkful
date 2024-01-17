---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Javascript: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qué y Por qué?
Eliminar caracteres que coinciden con un patrón es una técnica utilizada por los programadores para eliminar específicamente caracteres que cumplen con cierta condición definida. Esto puede ser útil para limpiar o filtrar entradas de texto en un programa.

## Cómo:
Para eliminar caracteres que coinciden con un patrón, podemos utilizar el método `replace()` de Javascript junto con una expresión regular. Por ejemplo, para eliminar todos los espacios en blanco de una cadena de texto, podemos usar el siguiente código:
```Javascript
let texto = "Este es un texto con varios espacios en blanco."
let nuevoTexto = texto.replace(/ /g, "")
console.log(nuevoTexto)
```
Este código producirá la siguiente salida:
`Esteestextoconvariosespaciosenblanco.`

Para eliminar otros caracteres como signos de puntuación, podemos usar una expresión regular más compleja que incluya todos los caracteres que queremos eliminar.

## Profundizando:
La eliminación de caracteres que coinciden con un patrón se ha vuelto más fácil con el uso de expresiones regulares. Estas son secuencias de caracteres que representan un patrón de búsqueda y se utilizan en la mayoría de los lenguajes de programación modernos.

Además del método `replace()`, también es posible utilizar el método `split()` en conjunto con expresiones regulares para dividir una cadena de texto en partes y luego unirlas nuevamente sin los caracteres que deseamos eliminar.

## Consulta También:
- [Documentación de Mozilla sobre el método `replace()`](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/String/replace)
- [Documentación de Mozilla sobre expresiones regulares](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)