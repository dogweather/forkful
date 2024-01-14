---
title:    "Gleam: Ekstrahering av deltekster"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor
Å ekstrahere substringer i programmering kan være nyttig når du trenger å manipulere tekstrenger på en spesifikk måte. For eksempel, hvis du ønsker å hente ut et bestemt ord fra en setning, eller begrense lengden på en tekst, så kan du bruke substrings for å oppnå dette.

## Slik Gjør Du
For å ekstrahere en substring i Gleam, kan du bruke funksjonen `slice` sammen med ønsket tekststreng og start- og sluttindekser. For eksempel:

```Gleam
let tekst = "Hei, mitt navn er Ole"
let navn = slice(tekst, 13, 16)
```

I dette tilfellet vil verdien av `navn` være `Ole`, da vi har satt startindeksen til å være 13 (inkludert) og sluttindeksen til å være 16 (ikke inkludert).

Du kan også begrense lengden på substringen ved å bruke funksjonen `slice_with_length`, som tar inn en startindeks, lengde og en tekststreng. For eksempel:

```Gleam
let telefonnummer = "1234567890"
let begrenset_nummer = slice_with_length(telefonnummer, 3, 4)
```

I dette eksempelet vil verdien av `begrenset_nummer` være `123`, da vi har satt startindeksen til å være 3 og lengden til å være 4.

## Dykk Dypere
Når du ekstraherer substringer, er det viktig å være klar over at indeksene teller fra 0. Dette betyr at for eksempel at første bokstav i en tekststreng har indeks 0, andre bokstav har indeks 1, osv. Det kan også være nyttig å vite at du kan bruke negative indekser for å hente ut substringer fra slutten av tekststrengene. For eksempel:

```Gleam
let navn = "Mari"
let siste_bokstav = slice(navn, -1, 1)
```

I dette tilfellet vil verdien av `siste_bokstav` være `i`, da vi har satt startindeksen til å være -1 og lengden til å være 1.

## Se Også
- [Gleam dokumentasjon om substrings](https://gleam.run/book/std/substring.html)
- [En annen artikkel om å ekstrahere substringer i Gleam (engelsk)](https://dev.to/lportmann/extracting-substrings-in-gleam-4n6g)