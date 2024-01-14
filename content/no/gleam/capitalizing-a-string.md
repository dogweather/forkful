---
title:                "Gleam: Store bokstaver i en tekststreng"
simple_title:         "Store bokstaver i en tekststreng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kapitalisere en streng er en vanlig oppgave som kan være nødvendig i mange programmeringsscenarier. Dette kan inkludere å formatere brukerinput, generere rapporter, eller til og med bare for visuell estetikk. Det kan også være nyttig hvis du ønsker å sammenligne to strenger uten å ta hensyn til store og små bokstaver.

## Slik gjør du det

For å kapitalisere en streng i Gleam, kan du bruke funksjonen ```String.to_uppercase``` og sende inn strengen du ønsker å kapitalisere som argument. La oss se på et enkelt eksempel:

```
Gleam ==========

let streng = "hei hei"
let kapitalisert_streng = String.to_uppercase(streng)

IO.print(kapitalisert_streng)
```

Dette vil resultere i følgende output:

```
HEI HEI
```

Som du kan se, vil funksjonen endre alle små bokstaver i strengen til store bokstaver.

Det er også verdt å merke seg at denne funksjonen ikke bare begrenser seg til ASCII-tegn, men fungerer også med Unicode-tekst.

## Dykk dypere

I Gleam kan du også bruke funksjonen ```String.to_title_case``` for å kapitalisere en streng i tittel-sak. Dette vil gjøre at den første bokstaven i hvert ord i strengen er stor, mens resten er små. La oss se på et eksempel:

```
Gleam ==========

let streng = "dette er en tittel"
let tittel_streng = String.to_title_case(streng)

IO.print(tittel_streng)
```

Output blir:

```
Dette Er En Tittel
```

Som du kan se, blir alle ordene i strengen kapitalisert, men resten av bokstavene forblir små.

En annen nyttig funksjon for å kapitalisere strenger i Gleam er ```String.to_capital_case```, som vil gjøre at hver setning i en streng starter med en stor bokstav. La oss se på et siste eksempel:

```
Gleam ==========

let streng = "dette er en setning. dette er en annen setning."
let nyttig_streng = String.to_capital_case(streng)

IO.print(nyttig_streng)
```

Output blir:

```
Dette er en setning. Dette er en annen setning.
```

Som du kan se, vil hvert punktum starte en ny setning og førstebokstaven i hver setning vil bli stor.

## Se også

- Offisiell dokumentasjon for strengmanipulering i Gleam: https://gleam.run/documentation/tutorials/strings
- En interaktiv Gleam leksjon om strengmanipulering: https://lettstrygleam.com/strings.html 
- Github-kodeeksempler for å kapitalisere strenger i Gleam: https://github.com/gleam-lang/gleam/blob/master/lib/std/string/scr/convert.gleam