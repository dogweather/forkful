---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng betyr å bestemme antall tegn i den gitte strengen. Dette er en vanlig oppgave i programmering, da det gir oss muligheten til å manipulere og begrense strenger mer effektivt.

## Hvordan:

La oss ta en titt på hvordan vi kan finne lengden på en streng i Gleam. Legg merke til at, som en statisk typet språk, krever Gleam at vi er tydelige med typer vi håndterer.

```Gleam
import gleam/string

fn main() {
  let my_string = "Hei, Verden!"
  let length = string.length(my_string)
  io.println(to_string(length))
}
```

Denne koden vil utskrive "13", som er lengden på strengen "Hei, Verden!".

## Dypdykk

Finne lengden av en streng har vært en vanlig oppgave i programmeringsspråk i mange år. Dette skyldes at strenger er grunnleggende datatype i nesten alle moderne programmeringsspråk.

I Gleam, er `string.length` funksjonen vi bruker til å bestemme lengden på en streng. Denne funksjonen går gjennom hver bokstav i strengen én om gangen til den når slutten, og teller deretter antall bokstaver som er forbi. 

Det er alternativer til `string.length`. For eksempel, hvis vi jobber med en streng som er satt opp som en liste med tegn, kan vi bruke `list.length` funksjonen i stedet.

```Gleam
import gleam/list

fn main() {
  let my_list = ['H', 'e', 'i', ', ', 'V', 'e', 'r', 'd', 'e', 'n', '!']
  let length = list.length(my_list)
  io.println(to_string(length))
}
```

Denne koden vil også utskrive "13". 

## Se også

1. Gleam's String-modul dokumentasjon: https://hexdocs.pm/gleam_stdlib/gleam/string/
2. Artikkel om hvordan arbeide med strenger i Gleam: https://gleam.run/book/tour/strings.html
3. Gleam's List-modul dokumentasjon: https://hexdocs.pm/gleam_stdlib/gleam/list/