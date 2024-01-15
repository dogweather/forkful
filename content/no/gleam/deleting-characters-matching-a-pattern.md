---
title:                "Slette tegn som matcher et mønster."
html_title:           "Gleam: Slette tegn som matcher et mønster."
simple_title:         "Slette tegn som matcher et mønster."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster kan være nyttig når du ønsker å rense eller formatere tekstfiler, eller når du jobber med sensitiv informasjon som trenger å bli maskert. I denne artikkelen skal vi se på hvordan dette kan gjøres ved hjelp av Gleam-programmeringsspråket.

## Slik gjør du det

For å slette tegn som matcher et mønster trenger du å bruke standardbiblioteket til Gleam, spesielt funksjonen `String.replace` og `Regex.regex`. Først må du importere disse modulene:

```
import string
import regex
```

Så kan du bruke `String.replace` til å erstatte alle tegn som matcher det gitte mønsteret med en tom streng:

```
let tekst = "Dette er en hemmelig tekst123"
let mønster = regex.compile("[a-z]")
let renset_tekst = string.replace(tekst, mønster, "")
```

I dette eksempelet vil `renset_tekst` bli "123", siden alle små bokstaver fra a-z (inkludert æøå) er slettet fra teksten.

## Grav litt dypere

La oss ta en nærmere titt på hvordan `Regex.regex` fungerer. Den tar to argumenter: et mønster og eventuelle flagg som du kan bruke for å spesifisere søkekriterier. I eksemplet over brukte vi `[a-z]` som mønster, men her er noen andre eksempler:

- `"[0-9]"` vil matche alle tall
- `"[^a-z0-9]"` vil matche alt som ikke er i alfabetet eller tall
- `"[A-Z]{3}"` vil matche tre store bokstaver på rad

Du kan også bruke flagg for å gjøre søket ditt mer spesifikt, som for eksempel `i` for å ignorere store/små bokstaver eller `g` for å matche flere ganger i teksten.

## Se også

- Gleam sin offisielle dokumentasjon for mer informasjon om `String.replace` og `Regex.regex`: [https://gleam.run/stdlib/regex](https://gleam.run/stdlib/regex)
- En annen Gleam-artikkel om å manipulere tekstfiler: [https://gleam.run/articles/text-manipulation-revisited](https://gleam.run/articles/text-manipulation-revisited)
- En tutorial om hvordan du maskerer sensitiv informasjon ved hjelp av Gleam: [https://gleam.run/articles/encryption-in-gleam](https://gleam.run/articles/encryption-in-gleam)