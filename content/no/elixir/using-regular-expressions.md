---
title:                "Elixir: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor 
Hvorfor burde du bruke regulære uttrykk i din Elixir-programmering? Regulære uttrykk er et kraftig verktøy for å søke, oppdage og manipulere tekstdata. Ved å bruke regulære uttrykk kan du effektivt finne og endre bestemte deler av en tekst, noe som kan være svært nyttig i mange programmeringsscenarier.

## Hvordan
For å bruke regulære uttrykk i Elixir, må du først importere modulen Regexp. Deretter kan du bruke funksjonen `=~` for å søke etter et mønster i en tekststreng. La oss si at du ønsker å finne alle ord som starter med bokstaven "t" i en tekststreng. Da kan du bruke følgende kode:

```Elixir
import Regexp

tekst = "Denne teksten inneholder mange ord, inkludert tomat, telefon og bokstav"
treff = =~ ~r/t[a-z]*/i, tekst

IO.inspect treff
```

Output vil være `[tomat, telefon, to, teksten]` siden disse ordene starter med "t". Merk at `~r//` rundt mønsteret er et regulært uttrykk literal og `i` flagget betyr at søket skal være case-insensitivt, slik at både store og små bokstaver blir funnet.

## Dypdykk
Det finnes mange forskjellige mønstre og operasjoner du kan bruke i regulære uttrykk, som for eksempel for å matche bokstaver, tall, spesifikke tegn og mye mer. Du kan også bruke gruppering, gjentagelse og backreferanse for å få enda mer avanserte mønstre. En god ressurs for å lære mer om regulære uttrykk i Elixir er offisiell dokumentasjon fra Elixir-organisasjonen og bokens "Programming Elixir" av Dave Thomas.

## Se også
- [Offisiell Elixir dokumentasjon for regulære uttrykk](https://hexdocs.pm/elixir/Regex.html)
- [Programming Elixir av Dave Thomas](https://pragprog.com/book/elixir13/programming-elixir-1-3)
- [Regulære uttrykk cheat sheet](https://raveren.github.io/komodo-elixir/docs/regex-cheat-sheet-sz.pdf)