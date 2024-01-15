---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Ruby: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué
Hay varias razones por las cuales alguien podría querer eliminar caracteres que coincidan con un patrón específico en un programa de Ruby. Algunas posibles razones incluyen la necesidad de limpiar datos no deseados, crear una versión codificada de un archivo de texto o simplificar la manipulación de cadenas de texto.

## Cómo hacerlo
La eliminación de caracteres que coinciden con un patrón en Ruby se puede lograr utilizando la función `gsub`. Esta función reemplaza todas las coincidencias de un patrón con una cadena vacía, lo que efectivamente elimina esos caracteres. Aquí hay un ejemplo de cómo se vería esto en el código:

```Ruby
texto = "¡Hola mundo!"
texto.gsub(/o/, "")
```

Este código eliminaría todas las letras "o" del texto y produciría la salida `"¡Hla mund!"` en la consola.

## Profundizando
La función `gsub` también acepta expresiones regulares como patrones, lo que la hace muy poderosa en términos de manipulación de texto. Además, si solo se necesita eliminar el primer carácter que coincida con el patrón, se puede usar la función `sub` en su lugar. También es importante tener en cuenta que `gsub` puede ser encadenado para eliminar múltiples patrones a la vez.

## Ver también
Para obtener más información sobre la función `gsub` y otras funcionalidades de manipulación de cadenas en Ruby, asegúrese de consultar la documentación oficial de Ruby en [ruby-doc.org](https://ruby-doc.org/core-2.7.1/String.html).