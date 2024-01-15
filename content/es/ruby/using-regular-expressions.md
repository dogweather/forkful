---
title:                "Utilizando expresiones regulares"
html_title:           "Ruby: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar expresiones regulares en Ruby?

Las expresiones regulares son una herramienta útil para la manipulación y búsqueda de patrones en cadenas de texto. Al utilizar expresiones regulares en Ruby, los desarrolladores pueden ahorrar tiempo y esfuerzo al escribir código más limpio y eficiente.

## Cómo utilizar expresiones regulares en Ruby

Para utilizar expresiones regulares en Ruby, primero debemos incluir el módulo `Regexp` en nuestro código. Luego, podemos crear una expresión regular utilizando el formato `/patrón/`, donde "patrón" es el patrón que queremos buscar en una cadena de texto. A continuación, podemos utilizar el método `match()` para buscar coincidencias en la cadena de texto.

```Ruby
# Ejemplo de una expresión regular que busca coincidir con una dirección de correo electrónico
email_regex = /[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}/
email = "john.doe@example.com"
matches = email.match(email_regex)
puts matches[0] # Imprime "john.doe@example.com", la coincidencia encontrada
```

También podemos utilizar el método `=~` para verificar si una cadena de texto coincide con una expresión regular.

```Ruby
# Comprueba si una cadena de texto contiene números de teléfono del Reino Unido
phone_regex = /(\+44\s?|0)7\d{3}\s?\d{6}/
phone_number = "07555 123456"
if phone_number =~ phone_regex
  puts "Teléfono del Reino Unido válido"
else
  puts "Teléfono del Reino Unido no válido"
end
```

## Profundizando en el uso de expresiones regulares en Ruby

Además de los métodos mencionados anteriormente, Ruby también tiene una variedad de métodos integrados para trabajar con expresiones regulares, como `scan()`, `sub()`, `gsub()` y `split()`. Estos métodos son útiles para realizar tareas específicas, como buscar múltiples coincidencias, reemplazar texto y dividir una cadena en partes utilizando un delimitador.

También es importante tener en cuenta los diferentes metacaracteres y secuencias de escape que se pueden utilizar en las expresiones regulares en Ruby, como `^` para buscar al principio de una cadena, `$` para buscar al final de una cadena y `\d` para buscar un dígito.

Además, hay una forma más avanzada de crear expresiones regulares en Ruby utilizando el constructor `Regexp::new()`, que permite incluir opciones como ignorar mayúsculas y minúsculas y hacer coincidir múltiples líneas.

## Vea también

- [Guía de expresiones regulares de Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Documentación oficial de expresiones regulares en Ruby](https://ruby-doc.org/core-3.0.2/Regexp.html)
- [Tutorial interactivo de expresiones regulares en Ruby](https://regexone.com/lesson/introduction_abcs)