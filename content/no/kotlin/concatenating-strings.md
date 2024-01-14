---
title:                "Kotlin: Sammenkobling av strenger"
simple_title:         "Sammenkobling av strenger"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor
Dette er et vanlig spørsmål blant nybegynnere i programmering: hvorfor skal man samle sammen strenger? Svaret er enkelt - å concatenere, eller sammenføye, strenger lar deg sette sammen forskjellige deler av en tekststreng for å lage en komplett setning eller tekst. Dette gjør det enklere å generere dynamiske tekster eller å formatere tekster på en mer fleksibel måte.

## Slik gjør du det
For å concatenere strenger i Kotlin, bruker vi operatøren "+" mellom de ulike elementene vi ønsker å kombinere. La oss se på et enkelt eksempel:

```Kotlin
val navn = "Maren"
val alder = 25
val tekst = "Hei, mitt navn er " + navn + " og jeg er " + alder + " år gammel."
print(tekst)
```

Dette vil gi følgende utskrift: Hei, mitt navn er Maren og jeg er 25 år gammel.

Operatøren "+" kan også brukes mellom en streng og en annen datatype, som for eksempel et tall. I slike tilfeller vil Kotlin automatisk konvertere datatypen til en streng før concatenasjonen.

```Kotlin
val tall = 42
print("Svaret på alt er: " + tall)
```

Her vil utskriften bli: Svaret på alt er: 42.

## Dykk dypere
I tillegg til å bruke operatøren "+" kan vi også bruke funksjonen "plus()" for å concatenere strenger i Kotlin. Denne funksjonen tar imot en eller flere strenger som parametere og returnerer en ny streng som er resultatet av concatenasjonen.

```Kotlin
val fornavn = "Per"
val etternavn = "Hansen"
val fulltNavn = fornavn.plus(" ").plus(etternavn) //Merk at " " brukes for å legge til et mellomrom mellom fornavn og etternavn
print(fulltNavn)
```

Utskriften vil bli: Per Hansen.

Dersom du ønsker å concatenate flere strenger uten mellomrom i mellom, kan du heller bruke funksjonen "concat()" som tar imot en Collection av strenger.

## Se også
- [Offisiell Kotlin dokumentasjon om strings](https://kotlinlang.org/docs/reference/strings.html)
- [Kotlin string concatenation tutorial](https://www.baeldung.com/kotlin-string-concatenation)
- [Kotlin string manipulation functions](https://www.geeksforgeeks.org/kotlin-string-manipulations/)