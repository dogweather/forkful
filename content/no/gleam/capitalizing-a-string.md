---
title:                "Sette en streng i store bokstaver"
html_title:           "Gleam: Sette en streng i store bokstaver"
simple_title:         "Sette en streng i store bokstaver"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Stor Bokstav: En Guide for Gleam Programmering 

Gleam er et sterkt statisk typet, funksjonelt programmeringsspråk designet for å bygge pålitelige, robuste og effektive systemer. Ingenting er mer nyttig og pålitelig enn å formatere strenger, spesielt stor bokstav. 

## Hva & Hvorfor?
Capitaliserer en streng, ganske enkelt, betyr å konvertere det første tegnet i strengen til et stort symbol. Og hvorfor ville vi ville gjøre dette? Generelt hjelper det med lesbarheten av teksten, og skiller ut visse strenger i koden din.

## Hvordan:
Her er kodeeksempler på hvordan du gjøre det i Gleam.

```Gleam
import gleam/string.{from_utf8, slice, to_uppercase}

fn stor_bokstav(string: String) {
  let first_char = slice(string, 0, 1)
  let rest_chars = slice(string, 1, byte_size(string))

  to_uppercase(first_char) ++ rest_chars
}
```
Eksempelutgangen vil se slik ut:

```Gleam
sto_bokstav("hallo verden") 
// Output: "Hallo verden"
```
## Dyp Dykk
Historisk sett har stor bokstav i programmering blitt brukt til å vise brukerinput i en bedre formatert og lesbar form. Det er alternativer til å bruke stor bokstav metoden som vises her, som innebygde bibliotekfunksjoner i andre språk som `.toUpperCase()` i JavaScript. 

I Gleam, brukes `slice()` funksjonen for å splitte strengen i to deler: det første tegnet og resten av strengen. 'to_uppercase()' funksjonen brukes deretter til å gjøre om det første tegnet til stor bokstav, og strengene slås sammen igjen.

## Også Se
For mer informasjon, ta en titt på [Gleam string dokumentasjon](https://hexdocs.pm/gleam_stdlib/gleam/string.html).

Lykke til med din Gleam programmering!