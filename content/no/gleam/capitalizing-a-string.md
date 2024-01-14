---
title:    "Gleam: Stor bokstav i en streng"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

I Gleam-programmering, så kan det være nyttig å kunne gjøre små endringer på en streng. For eksempel, så kan man ønske å kapitalisere den første bokstaven i en streng for å få en penere utskrift.

## Hvordan

Det er enkelt å kapitalisere en streng i Gleam. Her er et eksempel på hvordan man kan gjøre det:

```Gleam
let tekst = "hei alle sammen"
let kapitalisert_tekst = String.capitalize(tekst)

// Output: "Hei alle sammen"
```

I dette eksempelet, så bruker vi funksjonen `capitalize` fra `String` biblioteket for å kapitalisere strengen `tekst`. Vi kan deretter lagre den kapitaliserte strengen i en variabel og bruke den som vi ønsker.

## Dypdykk

Det finnes ulike måter å kapitalisere en streng på, avhengig av hva som skal kapitaliseres og i hvilken kontekst. For eksempel kan man også kapitalisere hver enkelt bokstav i en streng ved hjelp av `String.map` funksjonen. Man kan også kombinere flere funksjoner for å få ønsket resultat.

## Se også

- [Gleam dokumentasjon for String](https://gleam.run/libraries/string/)
- [Offisiell Gleam nettside](https://gleam.run/)
- [Gleam på GitHub](https://github.com/gleam-lang/gleam)