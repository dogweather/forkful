---
title:    "Ruby: Encontrando la longitud de una cadena"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Antes de sumergirnos en cómo encontrar la longitud de una cadena en Ruby, es importante entender por qué es una habilidad útil en programación. En términos simples, la longitud de una cadena se refiere al número de caracteres que tiene una cadena. Conocer la longitud de una cadena puede ser útil para diversas tareas como validar la entrada de un usuario, formatear texto para una presentación visual y realizar operaciones matemáticas con cadenas numéricas.

## Cómo hacerlo
Para encontrar la longitud de una cadena en Ruby, utilizamos el método `length`. Este método es aplicable a cualquier objeto de cadena y devuelve el número de caracteres en esa cadena. Aquí hay un ejemplo de código:

```
```Ruby
cadena = "Hola, ¿cómo estás?"
puts cadena.length
```

Este código imprimirá `18` ya que la cadena tiene 18 caracteres en total. Podemos ver cómo esto sería útil para, por ejemplo, limitar el número de caracteres permitidos en una entrada de texto de un usuario.

## Profundizando
Ahora que ya sabemos cómo encontrar la longitud de una cadena, es importante entender cómo funciona esto detrás de escena. Ruby trata a todas las cadenas como objetos y el método `length` es realmente un método del objeto String. Por lo tanto, cuando llamamos al método `length` en una cadena, lo que estamos haciendo es llamar al método `length` del objeto String que contiene esa cadena. Este método cuenta los caracteres en la cadena y devuelve un número entero que representa la longitud.

## Ver también
- Guía de Ruby: https://www.ruby-lang.org/es/documentation/
- Método `length` en la documentación de Ruby: https://ruby-doc.org/core-2.7.1/String.html#method-i-length
- Ejemplos de uso del método `length`: https://www.geeksforgeeks.org/ruby-string-length-method-with-examples/