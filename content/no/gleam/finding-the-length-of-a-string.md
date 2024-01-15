---
title:                "Å finne lengden på en streng"
html_title:           "Gleam: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du er en nybegynnerprogrammerer, lurer du kanskje på hvorfor du trenger å finne lengden til en streng. Vel, svaret er ganske enkelt - det er en grunnleggende operasjon som kan være nyttig i mange programmeringsscenarier. Ved å finne lengden på en streng, kan du enkelt manipulere og behandle data, og få mer komplekse oppgaver til å bli mye enklere.

## Hvordan
For å finne lengden til en streng i Gleam, kan du bruke den innebygde funksjonen "str.len ()". Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Gleam
let my_string = "Hei, dette er en test!";
let length = str.len(my_string);
```

I dette tilfellet vil variabelen "length" inneholde verdien 24, som er lengden på strengen "Hei, dette er en test!". Så enkelt er det!

## Dykk dypere
Det er verdt å merke seg at denne funksjonen bare fungerer for ASCII-strenger. Hvis du ønsker å finne lengden på en UTF-8-streng, må du bruke en annen funksjon "str.byte_length ()". Denne funksjonen vil gi deg antall byte i strengen, som ikke alltid vil være det samme som antall tegn.

I tillegg er det viktig å huske at strenger også kan være tomme, noe som betyr at de ikke inneholder noen tegn. I slike tilfeller vil både "str.len ()" og "str.byte_length ()" returnere verdien 0.

## Se Også
- [Offisiell Gleam Dokumentasjon om Strings](https://gleam.run/docs/stdlib/string)
- [Konverter en streng til en liste av tegn i Gleam](https://gleam.run/docs/cookbook/string-to-char-list/)
- [Utforsk andre innebygde strengfunksjoner i Gleam](https://gleam.run/docs/stdlib/string-functions)