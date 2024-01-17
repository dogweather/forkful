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

## ¿Qué y por qué?
"Eliminar caracteres que coinciden con un patrón" es una tarea común para los programadores de Ruby. Consiste en eliminar cualquier carácter en una cadena que coincida con un patrón específico. Los programadores pueden hacer esto para limpiar datos, procesar entradas de usuario o transformar una cadena en un formato específico.

## ¿Cómo hacerlo?
Podemos utilizar el método `gsub` para eliminar todos los caracteres que coincidan con un patrón en una cadena. Por ejemplo, si queremos eliminar todas las vocales de una cadena, podemos hacer lo siguiente:
```Ruby
string = "Hola mundo"
string.gsub(/[aeiou]/, "")
```
El código anterior primero crea una nueva cadena con el contenido "Hola mundo" y luego usa el método `gsub` para eliminar todas las vocales. El resultado final sería la cadena "Hl mnd".

También podemos utilizar expresiones regulares para eliminar caracteres específicos de una cadena. Por ejemplo, si queremos eliminar todos los dígitos de una cadena, podemos hacer lo siguiente:
```Ruby
string = "1234Ruby5678"
string.gsub(/\d+/, "")
```
El resultado final sería la cadena "Ruby".

## En profundidad
El método `gsub` es parte de la clase `String` en Ruby, que es una de las razones por las que es tan conveniente de usar para eliminar caracteres que coinciden con un patrón. Este método también tiene una variante llamada `gsub!`, que modifica la cadena original en lugar de crear una nueva.

Hay otras formas de eliminar caracteres que coinciden con un patrón en Ruby, como el método `delete` o el operador `tr`. Sin embargo, `gsub` es la opción más común y versátil ya que nos permite utilizar expresiones regulares para especificar el patrón de caracteres que queremos eliminar.

## Ver también
- Documentación oficial de `gsub`: https://ruby-doc.org/core-3.0.0/String.html#method-i-gsub
- Tutorial sobre expresiones regulares en Ruby: https://www.ruby-lang.org/es/documentation/regular-expressions/
- Otros métodos para eliminar caracteres en Ruby: https://www.geeksforgeeks.org/ruby-string-delete-sub-and-gsub-method/