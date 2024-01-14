---
title:                "Kotlin: Ekstrahering av delstrenger"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger, når du jobber med tekstbehandling eller manipulering av data, kan det være nyttig å kunne ekstrahere deler av en streng. Dette kan være for å få tak i bestemte informasjon, fjerne uønskede tegn eller forenkle søk etter en viss sekvens av tegn. Å kunne ekstrahere substrings er et nyttig verktøy i din Kotlin programmeringsverktøykasse.

## Hvordan
For å ekstrahere substrings i Kotlin, kan vi bruke funksjonen ```substring()```. Denne funksjonen tar inn to parametere, startindeksen og slutindeksen til den ønskede substringen. La oss se på et eksempel:

```
val navn = "Emilie"
val fornavn = navn.substring(0, 3)
print(fornavn) // output: Emi
```

I dette tilfellet har vi en streng med navnet "Emilie". Ved å bruke ```substring()``` funksjonen, velger vi å dukke opp bare de tre første tegnene, som er "Emi". Vi kan også bruke negative tall for indeksene, som vil telle fra slutten av strengen. La oss se på et annet eksempel der vi fjerner siste bokstav i strengen:

```
val navn = "Emilie"
val etternavn = navn.substring(0, navn.length-1)
print(etternavn) // output: Emili
```

Her bruker vi ```length``` metoden for å få lengden på strengen og trekker deretter en for å få den nest siste indeksen. Dette vil resultere i at siste bokstav i strengen blir fjernet. Husk at indekser i Kotlin (og mange andre programmeringsspråk) starter på 0, så den første bokstaven vil ha indeks 0.

## Dypdykk
I tillegg til å bruke ```substring()``` funksjonen, har Kotlin også en egen type kalt ```CharSequence``` for å jobbe med strenger. Dette er en grensesnittstype som gjør det mulig å manipulere strenger på ulike måter. For eksempel kan vi bruke ```subSequence()``` metoden på en streng for å ekstrahere en del av den. La oss se på et eksempel:

```
val årstid = "Våren"
val deler = årstid.subSequence(1,4)
print(deler) // output: åre
```

I dette tilfellet bruker vi ```subSequence()``` metoden på strengen "Våren" for å få tak i tegnene fra indeks 1 til og med 3, som er "åre". Det er viktig å merke seg at denne metoden returnerer en CharSequence, så vi må kanskje konvertere det til en streng ved å bruke ```toString()``` metoden etterpå.

## Se også
- [Official Kotlin Documentation on Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String Cheat Sheet](https://medium.com/@ealade/handy-kotlin-string-cheat-sheet-6576dfc9eef3)
- [Kotlin Docs: CharSequence](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-char-sequence/index.html)