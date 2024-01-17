---
title:                "Mayúsculas en una cadena"
html_title:           "Elixir: Mayúsculas en una cadena"
simple_title:         "Mayúsculas en una cadena"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Capitalizar una cadena de texto significa convertir la primera letra de cada palabra en mayúscula, y las demás en minúscula. Los programadores suelen hacer esto para que la cadena sea más legible y cumpla con ciertos estándares de formato.

## Cómo hacerlo:
Se puede capitalizar una cadena utilizando la función `String.capitalize/1` de Elixir. Para ello, simplemente se debe pasar la cadena como argumento. Por ejemplo:
```
Elixir String.capitalize("hola mundo") 
# Output: "Hola mundo"
```
Si se quiere capitalizar todas las palabras de una cadena, se puede usar la función `String.capitalize_words/1`. Por ejemplo:
```
Elixir String.capitalize_words("hola mundo") 
# Output: "Hola Mundo"
```

## Profundizando:
La capitalización de cadenas tiene su origen en la convención de escribir los títulos de libros y nombres propios con la primera letra en mayúscula. En el mundo de la programación, esto ayuda a estandarizar el formato de las cadenas y facilitar su lectura para otros programadores. 

Otra forma de capitalizar una cadena de texto es mediante la función `String.upcase/1`, que convierte todas las letras en mayúscula. También se puede utilizar `String.downcase/1` para convertir todas las letras en minúscula.

## Ver también:
- [Documentación oficial de Elixir sobre cadenas](https://hexdocs.pm/elixir/String.html)
- [Tutorial de Elixir sobre formateo de cadenas](https://elixir-lang.org/getting-started/string-interpolation-and-formatting.html)