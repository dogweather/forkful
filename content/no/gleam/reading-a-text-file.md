---
title:                "Lese en tekstfil"
html_title:           "Gleam: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese innholdet i en tekstfil er en vanlig oppgave for å hente informasjon fra en ekstern kilde eller for å behandle data som allerede finnes på datamaskinen din. Det kan være nyttig for å automatisere repetitivt arbeid, hente relevant informasjon eller bare for å ha en oversikt over dataene dine.

## Hvordan

For å lese innholdet i en tekstfil i Gleam, bruker vi funksjonen `File.read` og angir filbanen som en streng. Her er et eksempel på hvordan dette kan se ut:

```Gleam
let filbane = "min_filsti.txt"
let filinnhold = File.read(filbane)
```

Vi kan deretter behandle informasjonen i filen som ønsket, for eksempel ved å skrive den ut med `Debug.inspect`:

```Gleam
Debug.inspect(filinnhold)
```

Dette vil gi oss en utskrift av alt innholdet i filen, inkludert linjeskift og annen formatering.

## Deep Dive

Når vi bruker `File.read` funksjonen, blir innholdet i filen automatisk konvertert til Gleam sin `List` datatype. Dette betyr at vi kan bruke alle de vanlige funksjonene for å håndtere lister for å behandle informasjonen i filen.

For å legge til litt interaktivitet i koden vår, kan vi også be brukeren om å angi filbanen manuelt ved hjelp av `IO.prompt` funksjonen:

```Gleam
let filbane = IO.prompt("Skriv inn filbanen:")
let filinnhold = File.read(filbane)
```

På denne måten kan vi gjøre koden vår mer fleksibel og tilpasse den til forskjellige brukeres behov.

## Se Også

- [Dokumentasjon for `File` modulen i Gleam](https://gleam.run/documentation/std_lib/file/)
- [Andre nyttige ressurser for å lære Gleam](https://gleam.run/resources/)