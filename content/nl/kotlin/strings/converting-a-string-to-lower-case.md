---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:36.255126-07:00
description: 'Hoe: Kotlin''s `toLowerCase()` functie zet alle tekens in een string
  snel om naar kleine letters. Hier is hoe je het gebruikt.'
lastmod: '2024-03-13T22:44:50.754588-06:00'
model: gpt-4-0125-preview
summary: Kotlin's `toLowerCase()` functie zet alle tekens in een string snel om naar
  kleine letters.
title: Een string omzetten naar kleine letters
weight: 4
---

## Hoe:
Kotlin's `toLowerCase()` functie zet alle tekens in een string snel om naar kleine letters. Hier is hoe je het gebruikt:

```kotlin
fun main() {
    val originalString = "Dit is een GemENgDe KaPeRteKsT!"
    val lowerCaseString = originalString.lowercase()

    println(lowerCaseString) // Output: dit is een gemengde kapertekst!
}
```
Roep `lowercase()` aan en je bent klaar. Hoofdletters bij input doen er niet toe; de output is volledig in kleine letters.

## Diepgaand
Kotlin heeft het wiel niet opnieuw uitgevonden voor het omzetten van strings naar kleine letters. Het is eigenlijk een veelvoorkomende functie in programmeertalen. Historisch gezien hebben functies zoals C's `tolower()` lang te maken gehad met hoofdletterconversie.

Nu, twee twistpunten bij het omzetten naar kleine letters: lokale instellingen en prestaties. Kotlin's `lowercase()` kan een `Locale` accepteren omdat, verrassing, karakterhoofdletters niet universeel zijn. Zo gedragen de Turkse puntige en puntloze 'I' zich uniek bij hoofdletterconversies.

Prestaties? In de meeste apps merk je het niet. Maar grootschalige tekstverwerking vraagt meer geheugen en tijd omdat strings in Kotlin onveranderlijk zijn. Wanneer je een string naar kleine letters omzet, krijg je een nieuwe string.

Oudgedienden herinneren zich `.toLowerCase()` — Kotlin geeft nu de voorkeur aan `lowercase()` voor duidelijkheid.

## Zie Ook
- Kotlin String Documentatie: [Kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- Voor tekstverwerking en geavanceerde hoofdlettermanipulatie, raadpleeg de `java.lang.String` API: [Oracle Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- Begrip van lokale instellingen en taalkundige eigenaardigheden: [Oracle Locale Docs](https://docs.oracle.com/javase/tutorial/i18n/locale/)
