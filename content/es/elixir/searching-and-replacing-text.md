---
title:                "Elixir: Buscando y reemplazando texto."
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Por qué utilizar la búsqueda y reemplazo de texto en Elixir

La búsqueda y reemplazo de texto es una tarea común en la programación. En lugar de actualizar manualmente cada instancia de un término en tu código, puedes utilizar la función de búsqueda y reemplazo de texto en Elixir para ahorrar tiempo y esfuerzo.

## Cómo utilizar la búsqueda y reemplazo de texto en Elixir

La función para buscar y reemplazar texto en Elixir es `String.replace/4`. Toma cuatro argumentos: la cadena de texto original, el término a buscar, el término a reemplazar y un contador (opcional) para indicar cuántas veces se debe realizar el reemplazo.

Veamos un ejemplo de cómo podríamos utilizar esta función:

```
iex> original_str = "Hola mundo"
"Hola mundo"

iex> nuevo_str = String.replace(original_str, "mundo", "amigos")
"Hola amigos"
```

En este ejemplo, reemplazamos la palabra "mundo" por "amigos" en la cadena original "Hola mundo".

También podemos utilizar expresiones regulares en lugar de texto estático para realizar reemplazos en cadenas. Por ejemplo:

```
iex> original_str = "En cualquier orden, las letras en un string son sólo símbolos."
"En cualquier orden, las letras en un string son sólo símbolos."

iex> nuevo_str = String.replace(original_str, ~r/string/, "cadena")
"En cualquier orden, las letras en un cadena son sólo símbolos."
```

## Profundizando en la búsqueda y reemplazo de texto en Elixir

Además de la función `String.replace/4`, Elixir también tiene otras funciones útiles para trabajar con cadenas, como `String.match?/2` y `String.split/2`. Estas funciones pueden ser combinadas para realizar reemplazos más complejos y específicos a tus necesidades.

Por ejemplo, si queremos reemplazar todas las letras "a" por "b" en una cadena, salvo aquellas que están seguidas por una letra "y", podemos utilizar una expresión regular junto con `String.split/2` y `String.join/2` de la siguiente manera:

```
iex> original_str = "manzana amarilla y alambre"
"manzana amarilla y alambre"

iex> nuevo_str = original_str
|> String.split(~r/a([^y])/, trim: true)
|> Enum.join("b")
"mbrznb byrillb y bylmbrb"
```

Como puedes ver, este enfoque nos permite personalizar aún más nuestras operaciones de búsqueda y reemplazo de texto.

# Ver también

- [Documentación de Elixir sobre la función `String.replace/4`](https://hexdocs.pm/elixir/String.html#replace/4)
- [Tutorial sobre expresiones regulares en Elixir](https://elixirschool.com/es/lessons/basics/pattern-matching/#expresiones-regulares)