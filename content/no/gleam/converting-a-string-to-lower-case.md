---
title:                "Gleam: Konvertere en streng til små bokstaver"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er en nyttig funksjon som kan hjelpe deg med å formatere tekst i en mer konsistent og lesbar form. Dette kan være spesielt nyttig når du skal sammenligne eller søke etter tekststrenger.

## Hvordan

For å konvertere en streng til små bokstaver i Gleam, kan du enkelt bruke `String.to_lower_case` funksjonen. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Gleam
let tekst = "Gleam programmering er gøy!"
let konvertert_tekst = String.to_lower_case(tekst)
// konvertert_tekst er nå "gleam programmering er gøy!"
```

Det er viktig å merke seg at denne funksjonen bare gjelder for engelske bokstaver. Hvis du har tekst på andre språk, kan det være lurt å bruke `String.to_lower` funksjonen som støtter flere språk.

## Dypdykk

Hvis du ønsker å gå dypere inn i hvordan `String.to_lower_case` funksjonen fungerer, kan vi se på kildekoden for å få en bedre forståelse. I denne funksjonen bruker Gleam Unicode-standarden for å konvertere store bokstaver til små bokstaver. Dette betyr at funksjonen støtter alle Unicode-bokstaver, som gjør den veldig allsidig.

## Se også

Her er noen nyttige ressurser for å lære mer om å konvertere tekst til små bokstaver i Gleam:

- [Gleam offisiell dokumentasjon](https://gleam.run/)
- [Gleam Stack Overflow-samfunn](https://stackoverflow.com/questions/tagged/gleam)
- [Gleam Discord-kanal](https://discord.gg/BYXUgj9)