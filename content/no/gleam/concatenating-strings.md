---
title:                "Sammenslåing av strenger"
html_title:           "Gleam: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor
Å konkatenering av strenger (string concatenation) er en vanlig og viktig teknikk i programmering. Det lar oss kombinere flere strenger til en enkelt streng, som er nyttig for å lage dynamiske tekster og å formatere data på en lesbar måte.

# Hvordan gjør du det
Å konkatenering av strenger i Gleam er enkelt med bruk av operatorer og funksjoner. La oss se på noen eksempler:

```Gleam
let navn = "Per"

let velkomst = "Hei " ++ navn

io.println(velkomst)
```

Output: Hei Per

Vi kan også legge til konstanter eller tall i strenger:

```Gleam
let alder = 25

let info = "Jeg er " ++ alder ++ " år gammel"

io.println(info)
```

Output: Jeg er 25 år gammel

Dette fungerer også med flere variabler eller konstanter på en gang:

```Gleam
let fornavn = "Ola"
let etternavn = "Nordmann"

let navn = fornavn ++ " " ++ etternavn

io.println(navn)
```

Output: Ola Nordmann

# Deep Dive
Gleam har også en innebygd funksjon for å konkatenering av strenger, kalt `String.concat`. Denne funksjonen tar inn en liste av strenger og konkatenerer dem sammen til en enkelt streng. Dette kan være nyttig hvis du trenger å konkatenerer et større antall strenger.

Du kan også bruke `String.append` for å legge til en streng på slutten av en annen streng.

Vi må også være oppmerksomme på at konkatenering av strenger kan ha en effekt på ytelsen til programmet vårt. Det er derfor viktig å vurdere om det er den beste løsningen for å kombinere tekster i ditt spesifikke tilfelle.

# Se også
- [Offisiell Gleam dokumentasjon](https://gleam.run/)
- [Gleam 101: En innføring i Gleam programmeringsspråk](https://medium.com/gleam-lang/gleam-101-a21f7c983bc5)
- [10 måter å bli bedre i Gleam på](https://medium.com/gleam-lang/10-ways-to-become-better-at-gleam-e29baf74150d)