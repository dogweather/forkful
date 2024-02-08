---
title:                "Interpolering av en streng"
date:                  2024-01-20T17:50:39.324365-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Strenginterpolering lar oss sette variabler eller beregnede verdier inn i tekst. Vi bruker det for å bygge dynamiske strenger enkelt.

## How to:
Elixir bruker `#{}` for å interpolere uttrykk i strenger:

```elixir
name = "Verden"
message = "Hei, #{name}!"
IO.puts message
```

Output:
```
Hei, Verden!
```

For beregninger inne i en streng:

```elixir
hours = 24
message = "En dag har #{hours * 60} minutter."
IO.puts message
```

Output:
```
En dag har 1440 minutter.
```

## Deep Dive
I Elixir implementeres strenginterpolering ved hjelp av binær-operatoren `<>` som fletter sammen bitstrengelementer. Interpolering er en syntaktisk bekvemmelighet som forvandles til denne operatøren under kjøring.

Historisk sett arver Elixir denne funksjonen fra Ruby, der den er også mye brukt. Alternativer til interpolering inkluderer sammenkjeding med `<>` eller bruk av `String.concat/2`. Men interpolering er mer leselig og foretrukket i de fleste tilfeller.

Elixir kompilerer strenger til UTF-8 kodete binærer. Når du interpolerer, slår Elixirkompilatoren sammen disse binærene i én sammenhengende rekkefølge, slik at det er effektivt selv med store strenger.

## See Also
- [Elixir's String module documentation](https://hexdocs.pm/elixir/String.html)
- [Introduction to Elixir's binary type](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Splicing binaries with `<>`](https://hexdocs.pm/elixir/String.html#<>/2)
