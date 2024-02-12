---
title:                "Een string met hoofdletters maken"
aliases:
- /nl/kotlin/capitalizing-a-string.md
date:                  2024-01-28T21:56:24.574599-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een string met een hoofdletter laten beginnen betekent dat je de eerste letter van elk woord omzet naar een hoofdletter. Programmeurs doen dit om tekst te formatteren, zodat namen, titels of UI-elementen er netjes en gestandaardiseerd uitzien.

## Hoe te:

In Kotlin kun je strings gemakkelijk met een hoofdletter laten beginnen. Hier is een snel voorbeeld:

```kotlin
fun main() {
    val text = "kotlin programmeren"
    val capitalizedText = text.split(" ").joinToString(" ") { it.capitalize() }
    println(capitalizedText)
}
```

Voorbeelduitvoer:
```
Kotlin Programmeren
```
Om alleen de eerste letter van een zin met een hoofdletter te laten beginnen:

```kotlin
fun main() {
    val zin = "hallo, kotlin enthousiasten!"
    val capitalizedSentence = zin.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    println(capitalizedSentence)
}

```

Voorbeelduitvoer:
```
Hallo, kotlin enthousiasten!
```

Let op dat `capitalize()` is afgekeurd. Gebruik `replaceFirstChar { it.titlecase() }` voor betere toekomstige compatibiliteit.

## Diepere Duik

De methoden om te capitaliseren zijn veranderd in Kotlin. `capitalize()` werd veel gebruikt maar is in onbruik geraakt ten gunste van `replaceFirstChar { it.titlecase() }`. Deze verandering maakt de code duidelijker over wat er gebeurt - het gaat niet alleen om het met een hoofdletter laten beginnen, maar om het vervangen van het eerste karakter door de equivalent in titelcasus.

Waarom strings met een hoofdletter laten beginnen? Het is vaak een kwestie van gebruikersinterface. Denk aan boektitels, namen of elke lijst waar je consistentie nodig hebt. Het helpt bij de leesbaarheid en esthetiek.

Alternatieven voor capitaliseren omvatten:
- `.toLowerCase()`: Voor het omzetten naar kleine letters.
- `.toUpperCase()`: Voor het omzetten naar allemaal hoofdletters.
- CSS in webontwikkeling: soms wordt tekst in de frontend met een hoofdletter geschreven.

Achter de schermen interageren capitalisatiefuncties met Unicode-karakters. Karakters hebben specifieke hoofdletterversies. Het gaat niet alleen om het wisselen van een 'a' naar een 'A', het gaat om het begrijpen van taalspecifieke regels.

Vergeet de locales niet. In het Turks, bijvoorbeeld, wordt 'i' met een hoofdletter 'İ' en niet 'I'. Dus als je het taalonafhankelijk doet, kan dat problemen opleveren in meertalige applicaties.

## Zie Ook:

- Kotlin documentatie over `replaceFirstChar`: [Kotlin replaceFirstChar](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first-char.html)
- Unicode capitalisatieregels: [Richtlijnen voor Unicode Capitalisatie](http://unicode.org/versions/Unicode9.0.0/ch03.pdf#G33992)
- Capitalisatie in verschillende locales: [Locatie-Specifieke Capitalisatie](https://garygregory.wordpress.com/2015/11/03/java-lowercase-conversion-turkey/)
