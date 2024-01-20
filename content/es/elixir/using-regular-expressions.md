---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Las expresiones regulares son pautas que permiten a los programadores hacer coincidir, buscar y manejar texto. Los programadores las utilizan para validar entradas, buscar y reemplazar texto, y dividir cadenas.

## Cómo hacer:

Usando el módulo Regex en Elixir, puedes compilar, ejecutar e incluso reemplazar coincidencias de expresiones regulares en cadenas de texto. Aquí te enseñaremos cómo.

```elixir
#Compilando una expresión regular usando Regex.compile.
{:ok, regex} = Regex.compile("~r{[a-z]}")
#=> {:ok, ~r/[a-z]/}

#Encontrando una coincidencia con Regex.match?
Regex.match?(regex, "elixir")
#=> true

#Reemplazando texto con Regex.replace
Regex.replace(~r{a}, "Elixir", "x")
#=> "Elixir"
```
## Inmersión Profunda:

Las expresiones regulares tienen una larga historia que se remonta a los años 50 del siglo pasado, fueron popularizadas en los años 70 y hasta hoy día son una parte indispensable de la programación. En Elixir, los regex se importan del módulo Erlang llamado 're'.

Alternativas a `Regex` en Elixir son `String.contains?`, `String.match?` y `String.split`. Sin embargo, estas son menos eficientes para patrones de coincidencia complejos.

En cuanto a los detalles de implementación, Elixir se basa en la biblioteca de PCRE (Perl Compatible Regular Expressions) para hacer coincidir las expresiones regulares, lo que significa que es bastante rápido y puede trabajar con multitud de patrones de búsqueda.

## Ver También:

Para obtener más información, consulta la documentación oficial de Elixir:

- [`Regex` module documentation](https://hexdocs.pm/elixir/Regex.html)
- [Elixir Guides on `Strings and Binaries`](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Erlang's `re` module documentation](http://erlang.org/doc/man/re.html)