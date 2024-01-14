---
title:                "Elixir: Leyendo argumentos de línea de comandos"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Los argumentos de línea de comando son una herramienta importante para los programadores de Elixir. Leer los argumentos de línea de comando puede ayudarte a crear programas que sean más dinámicos y adaptables a diferentes escenarios.

## Cómo hacerlo

La forma más sencilla de leer los argumentos de línea de comando en Elixir es utilizando la función `System.argv()`. Esta función devuelve una lista de cadenas que contienen los argumentos ingresados por el usuario al momento de ejecutar el programa.

Veamos un ejemplo de cómo usarlo en un programa simple que toma dos argumentos y los suma:

```Elixir
#!/usr/bin/env elixir

defmodule Sumador do
  def sumar() do
    argumentos = System.argv()
    primer_numero = String.to_integer(argumentos[1])
    segundo_numero = String.to_integer(argumentos[2])
    resultado = primer_numero + segundo_numero

    IO.puts("La suma de los dos números es: #{resultado}")
  end
end

Sumador.sumar()
```

Si ejecutamos este programa desde la línea de comando con los argumentos `4` y `5`, el resultado será `La suma de los dos números es: 9`.

## Profundizando

Además de la función `System.argv()`, también podemos utilizar la biblioteca `OptionParser` para leer y manejar los argumentos de línea de comando de una manera más estructurada.

Por ejemplo, si queremos permitirle al usuario ingresar un flag `-r` que indique si queremos sumar o restar los números, podemos utilizar `OptionParser` de la siguiente manera:

```Elixir
require OptionParser

OptionParser.parse("-r", "Primer número", "Segundo número") do
  {["add", primer_numero, segundo_numero], _, _} ->
    resultado = primer_numero + segundo_numero
    IO.puts("La suma de los dos números es: #{resultado}")
  {["sub", primer_numero, segundo_numero], _, _} ->
    resultado = primer_numero - segundo_numero
    IO.puts("La resta de los dos números es: #{resultado}")
end
```

De esta manera, si ejecutamos nuestro programa con los argumentos `add 4 5`, obtendremos `La suma de los dos números es: 9`, mientras que si ejecutamos `sub 5 3`, obtendremos `La resta de los dos números es: 2`.

## Ver también

- [Documentación de `System.argv/0` en el sitio oficial de Elixir](https://hexdocs.pm/elixir/System.html#argv/0)
- [Documentación de `OptionParser` en el sitio oficial de Elixir](https://hexdocs.pm/elixir/OptionParser.html)
- [Tutorial de Elixir: Reading Command Line Arguments](https://elixir-lang.org/getting-started/command-line.html)