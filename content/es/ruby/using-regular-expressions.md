---
title:                "Uso de expresiones regulares"
date:                  2024-01-19
html_title:           "Arduino: Uso de expresiones regulares"
simple_title:         "Uso de expresiones regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Las expresiones regulares (regex) son patrones usados para encontrar coincidencias o reemplazar textos. Los programadores las utilizan por su potencia y flexibilidad para manejar cadenas de caracteres y automatizar tareas de validación y análisis.

## How to:
Aquí tienes ejemplos sencillos que muestran cómo utilizar expresiones regulares en Ruby:

```Ruby
# Búsqueda básica de un patrón
texto = "Hola, mundo!"
patron = /mundo/
coincide = texto.match(patron)
puts coincide # => #<MatchData "mundo">

# Reemplazo de texto
nuevo_texto = texto.sub(patron, "Ruby")
puts nuevo_texto # => "Hola, Ruby!"

# Validar formato de email
email = "usuario@example.com"
email_patron = /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i
valido = email.match(email_patron) ? "válido" : "inválido"
puts "El email es #{valido}" # => "El email es válido"
```

## Deep Dive:
Las expresiones regulares en Ruby se inspiran en Perl, conocidas por su eficacia. Existen alternativas como `String#scan` para encontrar todas las ocurrencias de un patrón o `String#gsub` para reemplazarlas. Al implementar regex, Ruby compila los patrones a un bytecode interno, optimizando su ejecución.

## See Also:
- [Documentación oficial de Ruby sobre Regexp](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Rubular: Un editor de expresiones regulares para Ruby](http://rubular.com/)
- [Learn Ruby Regexp](https://learn.co/lessons/ruby-regex)
