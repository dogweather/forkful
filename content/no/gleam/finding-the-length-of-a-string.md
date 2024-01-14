---
title:    "Gleam: Å finne lengden av en streng"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å finne lengden på en streng er en grunnleggende programmeringsoppgave som ofte brukes i dataanalyse og tekstbehandling. Ved å lære å finne lengden på en streng, vil du kunne utføre mer komplekse oppgaver som å finne substring og sammenligne strenger. 

## Hvordan 
For å finne lengden på en streng i Gleam trenger du bare følgende kode: 

```Gleam
let streng = "Hei, verden!"
io.format("Lengden på strengen er {}", std.string.length(streng))
```
Dette vil gi følgende utdata:
```
Lengden på strengen er 13
```
Som du kan se, bruker vi funksjonen `length` fra `std.string` biblioteket for å finne lengden på strengen. Denne funksjonen tar inn en streng som argument og returnerer lengden som et heltall, som vi kan skrive ut ved hjelp av `io.format`-funksjonen.

## Dykk dypere
Hvis du ønsker å forstå hvordan funksjonen `length` fungerer under overflaten, kan du utforske koden til `std.string` biblioteket. Her vil du finne at funksjonen `length` bruker en løkke for å telle antall tegn i strengen og deretter returnere dette tallet som et resultat.

## Se også
- [Offisiell Gleam dokumentasjon](https://gleam.run/documentation/)
- [Gleam på GitHub](https://github.com/gleam-lang/gleam)