---
title:                "Conversión de una cadena a minúsculas"
html_title:           "Elixir: Conversión de una cadena a minúsculas"
simple_title:         "Conversión de una cadena a minúsculas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Convertir una cadena de caracteres a minúsculas es un proceso común en la programación. Consiste en transformar todas las letras mayúsculas en minúsculas dentro de una cadena de texto. Los programadores utilizan esto para estandarizar el formato de texto y facilitar la manipulación de cadenas de caracteres.

## Cómo:
```Elixir
string = "HELLO WORLD"
String.downcase(string)
# Output: "hello world"
```

En este ejemplo, utilizamos la función `downcase` del módulo `String` para convertir la cadena `"HELLO WORLD"` a minúsculas.

## Profundizando:
Este proceso de conversión se basa en el código ASCII, en el que cada letra tiene un valor numérico asociado. En Elixir, también podemos utilizar la función `String.downcase/1` para realizar esta conversión de forma más eficiente y con soporte para caracteres no-ascii.

Existen también otras opciones para convertir una cadena a minúsculas, como por ejemplo utilizar expresiones regulares o utilizar una función recursiva. Sin embargo, la función `String.downcase/1` es la opción recomendada por su eficiencia y soporte para diferentes idiomas.

Es importante tener en cuenta que en Elixir las cadenas de caracteres son inmutables, por lo que cada vez que se realiza una conversión a minúsculas se crea una nueva cadena en memoria.

## Ver también:
- Documentación oficial de la función `String.downcase/1` https://hexdocs.pm/elixir/String.html#downcase/1
- Ejemplos de uso de expresiones regulares en Elixir https://hexdocs.pm/elixir/Regex.html