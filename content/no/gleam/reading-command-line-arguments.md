---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Gleam: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Det å lese kommandolinjeargumenter er en viktig del av programmering, spesielt for å gjøre programmene våre mer fleksible. Med kommandolinjeargumenter kan vi gi inndata direkte til programmet vårt når vi kjører det, i stedet for å måtte endre koden for hver gang vi ønsker å endre inndata.

## Slik gjør du det:
Vi kan lese kommandolinjeargumenter ved hjelp av Gleam sin `args` funksjon. Dette returnerer en liste med argumentene som ble gitt da vi kjørte programmet. La oss se på et eksempel:

```Gleam
// Sett variabel for kommandolinje-argumenter
let args = args

// Skriv ut alle argumentene
IO.print("Kommandolinje-argumenter:")
for arg in args do
  IO.print(arg)
```

Output av dette programmet når det kjøres med argumentene "hello" og "world" ville vært:

```
Kommandolinje-argumenter:
hello
world
```

## Dykk dypere:
Kommandolinjer har vært en del av programmering siden de tidlige dagene, og det finnes flere måter å lese dem på. I tillegg til å bruke `args` funksjonen, kan vi også bruke `std.os` modulen for å få tilgang til operativsystemets kommandolinje-argumenter direkte.

## Se også:
For mer informasjon om Gleam sin `args` funksjon og andre nyttige funksjoner, sjekk ut Gleam sin offisielle dokumentasjon: https://gleam.run/documentation.