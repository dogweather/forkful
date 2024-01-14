---
title:    "Kotlin: Å finne lengden til en streng"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor
Å finne lengden på en streng er en vanlig oppgave i programmering, uansett språk. Det kan være nødvendig for å validere brukerinput, manipulere tekst eller for generell informasjonsbehandling. I denne bloggposten vil jeg vise deg noen enkle måter å finne lengden på en string i Kotlin.

## Hvordan
For å finne lengden på en streng i Kotlin, kan du bruke den innebygde funksjonen "length". Dette vil returnere antall tegn i strengen, inkludert mellomrom. La oss se på et eksempel:

```Kotlin
val tekst = "Hei, dette er en streng"
println(tekst.length)

// Output: 25
```
Som du kan se, returnerte "length" funksjonen antall tegn i strengen vår. Men hva om du ønsker å utelate mellomrom? Da kan du bruke funksjonen "trim" først, som fjerner alle mellomrom på starten og slutten av strengen. La oss se på et eksempel på det også:

```Kotlin
val tekst = " Hei, dette er en streng "
val trimmetTekst = tekst.trim()

println(trimmetTekst.length)

// Output: 23
```
Som forventet, returnerer nå "length" funksjonen lengden på teksten uten mellomrom.

## Dypdykk
Å finne lengden på en streng kan virke enkelt, men det er noen ting som er verdt å merke seg. For det første, "length" funksjonen returnerer alltid et heltall, uavhengig av om strengen inneholder bokstaver, tall eller andre spesialtegn.

I tillegg bør du være oppmerksom på at det finnes forskjellige måter å måle lengden på en streng på. For eksempel kan språk som bruker Unicode, som norsk, ha tegn som tradisjonelt blir ansett som to separate tegn i andre språk. Dette kan føre til uventede resultater når du bruker "length" funksjonen. For å få nøyaktig antall tegn, kan du bruke "count" funksjonen, som tar med Unicode-tegn i beregningen. Se følgende eksempel:

```Kotlin
val tekst = "Å være eller ikke være"
println(tekst.count())

// Output: 22
```

## Se også
- [Dokumentasjon for "length" funksjonen i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
- [Dokumentasjon for "trim" funksjonen i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/trim.html)
- [Dokumentasjon for "count" funksjonen i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/count.html)