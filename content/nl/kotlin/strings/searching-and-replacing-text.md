---
title:                "Tekst zoeken en vervangen"
aliases: - /nl/kotlin/searching-and-replacing-text.md
date:                  2024-01-28T22:07:16.730318-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Tekst zoeken en vervangen is als verstoppertje spelen met strings, om vervolgens de verstopper te ruilen met iemand anders. Het is een veelvoorkomende programmeertaak, essentieel voor taken zoals bulkbewerking, gegevenssanering en het automatiseren van saaie klusjes.

## Hoe:
Kotlin vereenvoudigt tekstmanipulatie door middel van zijn standaardbibliotheek. Hieronder zie je hoe je `replace` gebruikt om woorden te wisselen.

```kotlin
fun main() {
    val originalText = "Kotlin is leuk, Kotlin is pragmatisch!"
    val newText = originalText.replace("pragmatisch", "cool")

    println(newText) // Output: Kotlin is leuk, Kotlin is cool!
}
```

Voor regex-patronen:

```kotlin
fun main() {
    val regex = "Kotlin".toRegex()
    val originalText = "Kotlin is leuk, Kotlin is pragmatisch!"
    val newText = regex.replace(originalText, "Java")

    println(newText) // Output: Java is leuk, Java is pragmatisch!
}
```

## Diepere Duik
Tekst herschrijven is zo oud als de druk, maar in programmeren nam het toe met vroege tekstverwerkers. Alternatieven? Zeker – zoek & vervang functies in editors, command-line tools zoals `sed`. In Kotlin specifiek heb je regex en eenvoudige stringmethodes tot je beschikking.

`replace` is rechttoe rechtaan voor simpele tekst; `Regex` geeft je een Zwitsers zakmes voor patronen. Regexen zijn krachtig maar lastiger – ze gebruiken speciale syntaxis voor patroonmatching. Denk aan regex als het spelen van Waar is Wally, maar je stelt de regels op over wat Wally draagt.

Implementatieproblemen? Onthoud dat Kotlin's `String` onveranderlijk is. Methoden die tekst wijzigen, retourneren nieuwe strings; ze veranderen het origineel niet.

## Zie Ook
- Kotlin-documentatie over `replace`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- Regex in Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- Goede oude `sed`: https://www.gnu.org/software/sed/manual/sed.html
