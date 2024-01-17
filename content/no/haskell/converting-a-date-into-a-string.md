---
title:                "Konvertere en dato til en streng"
html_title:           "Haskell: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Konvertering av en dato til en streng er en vanlig oppgave i programmering. Dette betyr å ta en dato i et bestemt format og gjøre den om til et tekstformat som kan leses og manipuleres av dataprogrammer. Programmere gjør dette for å få mer fleksibilitet og kontroll over datoer i sine programmer.

# Hvordan:
For å konvertere en dato til en streng i Haskell, bruker man funksjonen show. Denne funksjonen tar inn en verdi og returnerer den som en streng.

```Haskell 
show 2020-04-28
```

Dette vil gi outputen "2020-04-28" som en streng.

# Dypdykk:
Konvertering av datoer til strenger har vært en viktig del av programmering siden digitale datamaskiner ble utviklet. Før dette ble datoer registrert og lagret manuelt, og det var derfor ikke nødvendig å konvertere dem til strenger. I dag finnes det flere alternativer for å konvertere datoer til strenger, som for eksempel å bruke en innebygd funksjon eller å implementere sin egen funksjon basert på spesifikke behov.

Implementasjonen av funksjonen show i Haskell er basert på konvertering av verdier til strenger gjennom bruken av typeklassen Show. Dette gjør det mulig å konvertere forskjellige datatyper til strenger på en enhetlig måte.

# Se også:
For mer informasjon om konvertering av datoer til strenger i Haskell, sjekk ut følgende kilder:
- [Haskell Documentation on Show class](https://www.haskell.org/onlinereport/derived.html#sect20.3)
- [Haskell Reference on show function](https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:show)