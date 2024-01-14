---
title:                "Gleam: Å lese en tekstfil"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Velkommen! Visste du at å lese en tekstfil kan være en grunnleggende kode-oppførsel i enhver programmeringsverden? Det kan hjelpe deg å få tilgang til eller behandle informasjon som er lagret i en fil, for eksempel konfigurasjonsfiler eller datafiler. Uansett om du er nybegynner eller en erfaren Gleam-programmerer, er det viktig å forstå hvordan du kan lese en tekstfil for å kunne utnytte Gleam-potensialet fullt ut.

## Hvordan

For å lese en tekstfil i Gleam, følg disse trinnene:

1. Åpne tekstfilen ved hjelp av en filbehandler eller en terminal.
2. Merk deg filplasseringen og filnavnet.
3. Åpne en teksteditor og skriv følgende kode:

```Gleam
let tekstfil = std.io.file.read("stiftplassering/filnavn.txt")
```

4. Erstatt `stiftplassering` og `filnavn` med den faktiske fila du vil lese.
5. Kjør koden og sjekk resultatet.

Koden over bruker `std.io.file.read`-funksjonen til å lese filen og lagre innholdet i en variabel kalt `tekstfil`. Du kan da bruke variabelen til å manipulere eller bruke dataene som er lagret i filen.

## Dypdykk

Å lese en tekstfil kan være enkel, men det er viktig å forstå noen av begrensningene og mulighetene.

En begrensning er at Gleam ikke støtter direkte lesing av binære filer. Dette betyr at hvis tekstfilen din inneholder binær data, slik som bilder eller lydfiler, må du bruke et annet bibliotek eller åpne filen i en binær modus for å kunne lese den.

På den annen side er det noen nyttige funksjoner du kan bruke når du leser en tekstfil i Gleam. For eksempel kan du bruke `std.string.split`-funksjonen til å dele innholdet i filen basert på en separator. Dette er nyttig når du har strukturert data i filen og ønsker å behandle dem enkeltvis.

Husk at du også kan kombinere å lese en tekstfil med andre Gleam-funksjoner for å skape mer avansert funksjonalitet.

## Se også

- [std.io.file modul](https://gleam.run/docs/standard-library/io#file-module)
- [std.string modul](https://gleam.run/docs/standard-library/string)
- [Gleam Community forum](https://community.gleam.run)