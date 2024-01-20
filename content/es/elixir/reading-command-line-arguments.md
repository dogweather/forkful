---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lea Argumentos de la Línea de Comandos en Elixir

## ¿Qué & Por Qué?

Leer argumentos de la línea de comandos significa tomar aquí esos valores insertados cuando se ejecuta un programa desde la línea de comandos. Esta práctica es útil para personalizar la funcionalidad del programa.

## Cómo Hacerlo

Elixir facilita la lectura de argumentos de la línea de comandos con la función `System.argv/0`. Aquí hay un ejemplo de cómo utilizarlo:

```elixir
IO.inspect(System.argv())
```

Si pasas los argumentos `foo` y `bar` al script:

```shell
$ elixir my_script.exs foo bar
```

El script imprimirá:

```shell
["foo", "bar"]
```

## Inmersión Profunda

Los argumentos de la línea de comandos han sido una característica estándar de las interfaces de programación desde las primeras computadoras modernas, permitiendo la entrada de datos de usuario desde el inicio de un programa.

Por supuesto, existen alternativas para recoger los datos del usuario en Elixir. Por ejemplo, puedes usar `IO.gets/2` para leer la entrada del usuario en tiempo real.

La implementación de la lectura de argumentos de línea de comandos en Elixir es bastante directa. `System.argv/0` devuelve una lista de cadenas de texto que representan cada argumento pasado a la línea de comandos.

## Ver Además

Para profundizar más en este tema, puedes consultar las siguientes fuentes:
- Para familiarizarte más con Elixir y la línea de comandos, visita la [guía de inicio de Elixir](https://elixir-lang.org/getting-started/introduction.html).
- Para obtener más detalles sobre `System.argv/0`, consulta la [documentación oficial de Elixir](https://hexdocs.pm/elixir/System.html#argv/0).
- Para entender más acerca del Input/Output en Elixir, puede visitar [IO en la documentación oficial de Elixir](https://hexdocs.pm/elixir/io.html).