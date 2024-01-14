---
title:    "Elixir: Capitalizar una cadena"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por qué

Capitalizar una cadena de caracteres es una tarea común en la programación y puede ayudar a mejorar la legibilidad de un código. Si estás trabajando en un proyecto donde necesitas mostrar datos en mayúsculas, o simplemente quieres dar una mejor apariencia a tus mensajes de error, capitalizar una cadena es una gran opción.

## Cómo hacerlo

Para capitalizar una cadena en Elixir, podemos utilizar la función `capitalize/1`, que recibe como parámetro la cadena que queremos capitalizar. A continuación, un ejemplo de cómo utilizar esta función en un programa sencillo:

```Elixir
defmodule Main do
  def capitalize_example do
    input = "ejemplo de cadena"
    output = capitalize(input)
    IO.puts output
  end
end

Main.capitalize_example()

# Output: Ejemplo de cadena
```

Como se puede observar, la función `capitalize/1` toma la cadena "ejemplo de cadena" y la convierte a "Ejemplo de cadena". Esta función también se puede utilizar en cadenas que ya contengan mayúsculas, ya que no las afectará.

Otra opción para capitalizar una cadena es utilizar la función `String.capitalize/1`, que funciona de la misma manera que `capitalize/1`. Sin embargo, esta función también tiene en cuenta la regla de capitalización de palabras compuestas. A continuación, un ejemplo de cómo se comportaría `String.capitalize/1` en una cadena con una palabra compuesta:

```Elixir
defmodule Main do
  def capitalize_example do
    input = "cadena compuesta"
    output = String.capitalize(input)
    IO.puts output
  end
end

Main.capitalize_example()

# Output: Cadena Compuesta
```

Si bien ambos métodos producen el mismo resultado en cadenas simples, es importante tener en cuenta esta diferencia en casos de texto más complejos.

## Profundizando

En Elixir, las cadenas no se modifican directamente, sino que se crea una nueva cadena cada vez que se realiza una operación. Por lo tanto, cuando utilizamos la función `capitalize/1`, se crea una nueva cadena a partir de la original con el cambio necesario.

Además de `capitalize/1` y `String.capitalize/1`, también existen otras funciones como `String.upcase/1` y `String.downcase/1` que nos permiten convertir una cadena a mayúsculas o minúsculas, respectivamente.

## Ver también

- [Documentación de Elixir sobre cadenas](https://hexdocs.pm/elixir/String.html)
- [Tutorial de Elixir en español](https://elixirespanol.github.io/)
- [Ejercicios de Elixir en línea con Proyecto Euler](https://euler.codes/)