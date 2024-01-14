---
title:                "Elixir: Kapitalisering av en streng"
simple_title:         "Kapitalisering av en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvorfor det er viktig å kunne sette begynnelsen av en streng med store bokstaver? Det kan være flere grunner til dette, som for eksempel å gjøre tekst mer leselig eller å følge konvensjoner i et bestemt programmeringsspråk. Uansett årsak, her vil du lære hvordan du kan gjøre dette i Elixir.

## Hvordan

Det er enkelt å kapitalisere en streng i Elixir ved å bruke den innebygde funksjonen `String.capitalize/1`. La oss se på et eksempel:

```Elixir
IO.puts String.capitalize("dette er en test")
```

Output: "Dette er en test"
 
Som du kan se, har den første bokstaven i strengen blitt endret fra "d" til "D". Men hva skjer hvis vi allerede har en streng som starter med en stor bokstav? La oss prøve det samme eksempelet igjen:

```Elixir
IO.puts String.capitalize("Dette er en test")
```

Output: "Dette er en test"

Som du kan se, forblir bokstaven uendret, siden den allerede er stor. Dette er fordi funksjonen bare kapitaliserer den første bokstaven i strengen hvis den er en liten bokstav.

## Dypdykk

For å forstå hvordan `String.capitalize/1` fungerer, kan vi ta en titt på koden bak den. Her er en forenklet versjon av funksjonen:

```Elixir
def capitalize(string) do
  [first | rest] = string
  first = String.upcase(first)

  first <> rest
end
```

Som du kan se, deler funksjonen strengen inn i en liste av enkeltbokstaver. Deretter bruker den `String.upcase/1` for å konvertere den første bokstaven til stor bokstav. Til slutt, ved å bruke den binære operator `<>`, blir den kombinert med resten av strengen.

## Se også

- [Elixir standard library](https://hexdocs.pm/elixir/Kernel.html#String.capitalize/2)
- [String modulen i Elixir](https://hexdocs.pm/elixir/String.html)
- [Konvertere strenger i Elixir](https://www.phoenixframework.org/blog/leveraging-elixir-s-string-module)

*Takk for at du leste denne korte guiden til å kapitalisere strenger i Elixir. Håper du finner det nyttig i dine Elixir-prosjekter!*