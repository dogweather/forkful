---
title:                "Encontrando la longitud de una cadena."
html_title:           "Elixir: Encontrando la longitud de una cadena."
simple_title:         "Encontrando la longitud de una cadena."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez te has preguntado cuántos caracteres tiene una cadena en particular? Saber la longitud de una cadena es un concepto básico pero esencial en cualquier lenguaje de programación. Te permite manipular y trabajar con cadenas de manera eficiente y efectiva.

## Cómo hacerlo

```elixir
# Definir una cadena
cadena = "Hola mundo"

# Utilizar la función `String.length` para obtener la longitud
String.length(cadena) #=> 10

# También puede aplicarse a variables o expresiones
variable = "¡Elixir es increíble!"
String.length(variable) #=> 20

# Incluso puede utilizarse en cadenas multilínea
otra_cadena = """
Esta es una cadena
con varias líneas
"""
String.length(otra_cadena) #=> 28```

Puedes ver que la función `String.length` es fácil de usar y nos brinda rápidamente la longitud de cualquier cadena que le pasemos como argumento.

## Deep Dive

Ahora que ya sabemos cómo obtener la longitud de una cadena de manera básica, es importante entender cómo funciona internamente. En Elixir, las cadenas son caracteres codificados en la tabla de caracteres Unicode. Los caracteres pueden tener diferentes longitudes, dependiendo del conjunto de caracteres que esté utilizando. Por lo tanto, la función `String.length` cuenta el número de caracteres, no el número de bytes.

Además, es importante tener en cuenta que la función `String.length` solo cuenta los caracteres visibles. Por ejemplo, si tenemos una cadena que contenga un carácter de control especial, como un carácter de nueva línea, esa longitud no se contará. Esto se debe a que en la mayoría de los casos, estos caracteres no son visibles en la cadena.

## Ver también

- [Documentación oficial de Elixir sobre cadenas](https://hexdocs.pm/elixir/String.html)
- [Otra forma de obtener la longitud de una cadena en Elixir](https://stackoverflow.com/questions/30630131/how-can-i-find-the-length-of-a-string-in-elixir)
- [Ejercicios prácticos con cadenas en Elixir](https://github.com/ElixirCasts/strings)