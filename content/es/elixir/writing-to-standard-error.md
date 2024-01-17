---
title:                "Escribiendo en el estándar de error."
html_title:           "Elixir: Escribiendo en el estándar de error."
simple_title:         "Escribiendo en el estándar de error."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Escribir a la salida de error estándar es una forma en la que los programadores pueden imprimir mensajes de error mientras ejecutan su código. Esto les permite obtener información más detallada sobre los errores que ocurren durante la ejecución del programa.

Los programadores utilizan esto para depurar su código y encontrar y solucionar errores de forma más eficiente.

## Cómo:

```Elixir
IO.puts("Mensaje a la salida estándar")
```

Salida:

```Elixir
Mensaje a la salida estándar
```

## Profundizando:

Escribir a la salida de error estándar no es algo nuevo. Se remonta a las primeras etapas de la programación informática. En el pasado, los programadores escribían a una salida de error estándar para depurar sus programas y obtener información sobre posibles errores.

Hoy en día, escribir a la salida de error estándar sigue siendo una forma útil de depurar código y obtener información sobre posibles problemas en el mismo. Sin embargo, también existen otras alternativas, como los sistemas de registro, que permiten a los programadores almacenar y ver información de error de manera más organizada.

En Elixir, escribir a la salida de error estándar se realiza utilizando la función `IO.puts/2`. Puedes imprimir cualquier tipo de dato a la salida de error estándar utilizando esta función.

## Ver también:

- [Documentación oficial de Elixir sobre `IO.puts/2`](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Ejemplos de cómo utilizar `IO.puts/2`](https://elixir-lang.org/getting-started/io-and-the-file-system.html#io-and-the-file-system)
- [Más sobre la salida de error estándar y otras opciones de depuración en Elixir](https://elixirschool.com/es/lessons/basics/io-n-enumerables/)