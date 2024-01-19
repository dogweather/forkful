---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver involverer å endre alle store bokstaver i en gitt tekst til små bokstaver. Programmerere gjør dette ofte for å normalisere tekst for enklere databehandling og tekstmanipulering.

## Hvordan:
Her er eksempel på hvordan du kan konvertere en streng til små bokstaver i Kotlin:

```Kotlin
fun main() {
    val tekst = "JEG ELSKER NORGE!"
    val litenTekst = tekst.toLowerCase()
    println(litenTekst)
}
```

Output:

```Kotlin
jeg elsker norge!
```

## Dypere forståelse
- Historisk kontekst: Strengkonvertering har eksistert siden tidlige programmeringsspråk som BASIC. Det er en universell operasjon som finnes i de fleste programmeringsspråk.
- Alternativer: Du kan også bruke `String's` `decapitalize()` metoden i Kotlin, som bare konverterer den første bokstaven i strengen til små bokstaver.
- Impliserte detaljer: `toLowerCase()`-funksjonen i Kotlin bruker Unicode-data for å få en nøyaktig konvertering av tekst. Dette betyr at den kan håndtere språkspesifikke bokstaver, som Æ, Ø, og Å.

## Se også:
- Kotlin Dokumentasjon for `String`: [Link](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Utforsk `decapitalize()`: [Link](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/decapitalize.html)
- Kotlin i Dybden: [Link](https://www.packtpub.com/eu/mobile/kotlin-in-depth-video)