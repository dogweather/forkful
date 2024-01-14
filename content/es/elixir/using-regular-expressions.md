---
title:                "Elixir: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, las expresiones regulares son una herramienta muy útil para buscar patrones en una cadena de texto. Nos permiten extraer información específica, validar entradas y realizar operaciones de sustitución y manipulación de cadenas de manera eficiente. Con Elixir, podemos utilizar expresiones regulares de una forma sencilla y efectiva.

## Cómo hacerlo

Para utilizar expresiones regulares en Elixir, podemos utilizar la función `Regex.match?` junto con el operador `=~`. Por ejemplo, si queremos buscar todas las coincidencias de números dentro de una cadena, podemos usar el siguiente código:

```Elixir
cadena = "Hola, tengo 23 años y mi número de teléfono es 555-1234"
Regex.match?(~r/\d+/, cadena)
```

Esto nos devolverá `true` ya que la cadena contiene números en la posición 23 y 1234. También podemos utilizar expresiones regulares para realizar sustituciones con la función `Regex.replace` y utilizando grupos de captura. Por ejemplo, si queremos sustituir todas las letras por su correspondiente en mayúscula, podemos hacerlo de la siguiente manera:

```Elixir
cadena = "hola mundo"
Regex.replace(~r/[a-z]/, cadena, &(String.upcase(&1)), global: true)
```

Esto nos devolverá la cadena "HOLA MUNDO".

## Profundizando

Al trabajar con expresiones regulares en Elixir, podemos utilizar diferentes metacaracteres para crear patrones más complejos. Algunos de los más utilizados son `^` (para indicar el inicio de la cadena), `$` (para indicar el final de la cadena), `.` (para representar cualquier carácter), `+` (para indicar una o más repeticiones), entre otros. Además, también podemos utilizar modificadores como `i` para ignorar mayúsculas y minúsculas.

Es importante mencionar que las expresiones regulares en Elixir utilizan la sintaxis de Perl, por lo que si estás familiarizado con ella, te será aún más sencillo utilizarlas.

## Ver También

- [Documentación de expresiones regulares en Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Tutorial de expresiones regulares en Elixir](https://elixir-lang.org/getting-started/regex.html)
- [Expresiones regulares en Elixir: Un ejemplo práctico](https://dev.to/alexeih/tricks-with-regex-in-elixir-2e8j)