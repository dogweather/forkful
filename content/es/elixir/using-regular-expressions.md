---
title:                "Elixir: Utilizando expresiones regulares"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

Los patrones de expresiones regulares son herramientas muy útiles para encontrar y manipular cadenas de texto dentro de un código. Son especialmente útiles cuando se trabaja con grandes cantidades de datos y se necesita buscar patrones específicos de caracteres o palabras. Con el uso de expresiones regulares, se puede ahorrar mucho tiempo y esfuerzo en la manipulación de cadenas de texto.

## Cómo usar expresiones regulares en Elixir

Para utilizar expresiones regulares en Elixir, se debe primero importar el módulo `Regex` en el código. Luego, se pueden utilizar las diferentes funciones proporcionadas por el módulo para trabajar con patrones de expresiones regulares.

Por ejemplo, si queremos encontrar todas las coincidencias de un patrón específico en una cadena de texto, podemos utilizar la función `match` de la siguiente manera:

```Elixir
Regex.match("Este es un ejemplo de una cadena de texto", ~r{ejemplo})
```

El resultado de este código sería una coincidencia con la palabra "ejemplo", ya que es lo que se especificó en el patrón (`~r{ejemplo}`).

También se pueden utilizar otros operadores para buscar patrones, como `=~` para verificar si una cadena de texto cumple con un patrón específico, o `~s` para definir el patrón como una cadena de texto en lugar de una expresión regular.

## Profundizando en el uso de expresiones regulares

Elixir ofrece una gran variedad de funciones y operadores para trabajar con expresiones regulares. Además de `match`, también se pueden utilizar `split` para dividir una cadena de texto en diferentes partes basadas en un patrón, `replace` para reemplazar partes de una cadena de texto que coincidan con un patrón, y mucho más.

También es posible utilizar modificadores en los patrones para hacer que la búsqueda sea más específica. Por ejemplo, `~r{patrón}i` buscará coincidencias sin importar si las letras son mayúsculas o minúsculas.

Es importante recordar que las expresiones regulares pueden ser poderosas, pero también pueden ser complejas y difíciles de entender. Se recomienda practicar con diferentes patrones y funciones para familiarizarse con su uso y sacar el máximo provecho de ellos.

## Ver también

- [Documentación del módulo Regex en Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Introducción a expresiones regulares en Elixir](https://elixir-lang.org/getting-started/regex.html)
- [Tutorial de expresiones regulares en Elixir](https://www.tutorialspoint.com/elixir/elixir_regular_expressions.htm)