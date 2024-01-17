---
title:                "Skriver til standardfeil"
html_title:           "Gleam: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å skrive til standard error er en metode for å sende feilmeldinger og annen nyttig informasjon til en spesiell strøm som kalles "standard error". Programmere bruker dette for å få en bedre oversikt over feilene og for å kunne fange dem i koden.

## Hvordan:
```
Gleam.hvordan_skriver(shader)
Gleam.hvordan_skriver("Til standard error: Dette er en feilmelding")
```

Eksempel på utgang:
```
Feil: Til standard error: Dette er en feilmelding
```

## Dykk dypere:
Å skrive til standard error ble først brukt på Unix-systemer som en måte å skille mellom feilmeldinger og annen informasjon. Andre alternativer for å håndtere feil inkluderer å skrive til standard utgang eller å kaste unntak.

En implementasjonsdetalj å merke seg er at når man skriver til standard error, vil det legges til en linjeskift på slutten av meldingen, mens standard utgang vil beholde meldingen som den er.

## Se også:
- [Official Gleam Documentation](https://gleam.run/documentation/)
- [Gleam Github Repository](https://github.com/gleam-lang/gleam)