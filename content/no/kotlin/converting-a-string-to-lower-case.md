---
title:    "Kotlin: Konvertere en streng til små bokstaver"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Det er mange scenarier hvor man trenger å arbeide med tekststrenger i et program. Noen ganger kan det være nødvendig å konvertere tekst til små bokstaver for å forenkle sammenligning av tekst eller for å opprettholde et ensartet utseende. Heldigvis er dette en enkel oppgave å gjøre med Kotlin, og i denne bloggposten vil vi se nærmere på hvordan man konverterer en tekststreng til små bokstaver.

## Hvordan

For å konvertere en tekststreng til små bokstaver i Kotlin, kan man bruke funksjonen `toLowerCase()`. Denne funksjonen tar ingen parametere og returnerer en ny tekststreng med alle bokstavene i små bokstaver.

```Kotlin
val tekst = "Hei, dette er en TEKSTSTRENG"
val konvertertTekst = tekst.toLowerCase()
print(konvertertTekst)
```

Output: "hei, dette er en tekststreng"

Man kan også bruke `toLowerCase()` sammen med andre funksjoner for å manipulere tekststrenger. For eksempel kan man kombinere det med `split()` for å konvertere kun visse deler av en tekststreng til små bokstaver.

```Kotlin
val tekst = "Hei, dette er en TEKSTSTRENG"
val ord = tekst.split(" ")
val konverterteOrd = ord.map { it.toLowerCase() }
val konvertertTekst = konverterteOrd.joinToString(" ")
print(konvertertTekst)
```

Output: "hei, dette er en tekststreng"

## Dypdykk

Det er viktig å merke seg at `toLowerCase()` funksjonen bruker Unicode-standard for å konvertere bokstaver til små bokstaver. Dette betyr at den også vil fungere på bokstaver fra andre språk enn norsk. For eksempel vil den også konvertere bokstaven "Æ" til "æ".

Det finnes også en lignende funksjon kalt `toUpperCase()` som konverterer tekst til store bokstaver. Begge funksjonene kan være nyttige når man arbeider med tekststrenger i et program, og det er verdt å undersøke og eksperimentere med dem for å bli mer kjent med hvordan de fungerer.

## Se Også

- Dokumentasjon for `toLowerCase()` og `toUpperCase()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html
https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-upper-case.html