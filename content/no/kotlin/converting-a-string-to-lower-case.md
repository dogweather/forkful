---
title:    "Kotlin: Konvertere en streng til små bokstaver"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en streng til små bokstaver er en vanlig oppgave i mange programmeringsspråk, inkludert Kotlin. Det kan være nyttig for å sikre ensartethet i tekst eller for å sammenligne strenger uten å ta hensyn til store og små bokstaver.

## Hvordan
Du kan bruke innebygde metoder i Kotlin for å konvertere en streng til små bokstaver. Her er et enkelt eksempel:

```Kotlin
val navn = "Jonas"
val navnMedSmåBokstaver = navn.toLowerCase()
println(navnMedSmåBokstaver) // output: jonas
```

Merk at den opprinnelige strengen ikke endres, så det er viktig å lagre den konverterte strengen i en variabel for å kunne bruke den senere.

Du kan også bruke en alternativ metode som tar inn et språkkode-argument for å håndtere tilfeller der språket har spesielle regler for å konvertere bokstaver. Her er et eksempel som konverterer en tysk tekst til små bokstaver:

```Kotlin
val tekst = "Großer Apfel"
val tekstMedSmåBokstaver = tekst.toLowerCase(Locale.GERMAN)
println(tekstMedSmåBokstaver) // output: großer apfel
```

## Dypdykk
Bak kulissene bruker Kotlin Unicode-standard for å konvertere bokstaver til små eller store bokstaver. Dette betyr at språket er i stand til å håndtere ulike alfabeter og særegenheter i ulike språk.

Det kan også være nyttig å vite at Kotlin har en innebygd funksjon for å kun konvertere første bokstav i en streng til stor bokstav, som kan være praktisk for å formatere navn eller titler.

## Se også
- Dokumentasjon for `toLowerCase()`-metoden fra Kotlin Standard Library: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html
- Unicode-tabellen som Kotlin bruker for å konvertere bokstaver: https://unicode.org/charts/
- Oversikt over Kotlin Standard Library: https://kotlinlang.org/api/latest/jvm/stdlib/