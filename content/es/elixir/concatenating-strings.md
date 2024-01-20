---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenación de Cadenas en Elixir

## ¿Qué es y Por qué?

La concatenación de cadenas es la acción de unir dos o más cadenas en una sola. Los programadores la usan para formar contenidos de texto dinámicos o personalizar mensajes para el usuario.

## Cómo hacerlo:

En Elixir, utiliza el operador `<>`. Aquí tienes un ejemplo.

```Elixir
cadena_1 = "Hola, "
cadena_2 = "¿Cómo estás?"

IO.puts cadena_1 <> cadena_2
```

El resultado sería:

```
Hola, ¿Cómo estás?
```

Muy simple, ¿verdad?

## Un vistazo más profundo

Históricamente, Elixir se basa en Erlang, un lenguaje diseñado para sistemas distribuidos y paralelización. Por lo tanto, se centra en la eficiencia, incluso al manipular cadenas de texto. 
La concatenación mediante `<>` es eficiente, pero no es la única manera. Otras alternativas incluyen el uso de la interpolación de cadenas con `#{}`:

```Elixir
nombre = "José"
IO.puts "Hola, #{nombre}"
```

Este enfoque sigue siendo eficiente y mejora la legibilidad cuando se mezcla texto estático con variables.

Detalles de implementación: Las cadenas en Elixir están representadas internamente como listas de números binarios. Esto significa que la concatenación es tan sencilla como fusionar listas, operación que es rápida en Elixir.

## Más información:

Podrás encontrar más detalles en los siguientes recursos (todos en inglés):

- [Elixir Lang: String](https://hexdocs.pm/elixir/String.html)
- [Elixir Forum: How to concatenate strings?](https://elixirforum.com/t/how-to-concatenate-strings/346)