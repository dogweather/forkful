---
aliases:
- /nl/kotlin/creating-a-temporary-file/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:20.033308-07:00
description: "Een tijdelijk bestand aanmaken is het cre\xEBren van een bestand dat\
  \ bedoeld is om op korte termijn op je bestandssysteem te leven, vaak voor dingen\
  \ zoals\u2026"
lastmod: 2024-02-18 23:09:01.827107
model: gpt-4-0125-preview
summary: "Een tijdelijk bestand aanmaken is het cre\xEBren van een bestand dat bedoeld\
  \ is om op korte termijn op je bestandssysteem te leven, vaak voor dingen zoals\u2026"
title: Een tijdelijk bestand aanmaken
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tijdelijk bestand aanmaken is het creëren van een bestand dat bedoeld is om op korte termijn op je bestandssysteem te leven, vaak voor dingen zoals tussenliggende gegevens. Programmeurs doen dit voornamelijk omdat het kan helpen bij het beheren van ruimte, het verminderen van conflicten en het verhogen van de beveiliging tijdens runtime.

## Hoe te:
Hier is een snelle manier om een tijdelijk bestand te maken in Kotlin:

```Kotlin
import java.io.File

fun main() {
    val tempFile = File.createTempFile("myTempFile", ".tmp")

    println("Tijdelijk bestand aangemaakt op: ${tempFile.absolutePath}")

    // Schrijf naar tijdelijk bestand
    tempFile.writeText("Kotlin is best netjes, hè?")

    // Verwijder bij afsluiten
    tempFile.deleteOnExit()
}
```

De uitvoer zal iets zijn als:

```
Tijdelijk bestand aangemaakt op: /tmp/myTempFile1234567890.tmp
```

Je pad van het tijdelijke bestand zal verschillen. Het zal een unieke naam hebben, dus maak je geen zorgen over naamconflicten.

## Diepere Duik
De `File.createTempFile()` methode is goud waard voor ad-hoc bestandsgeneratie. Het is er al sinds de vroege dagen van Java en Kotlin, als een JVM-taal, maakt er volledig gebruik van.

Enkele alternatieven:
- `Files.createTempFile()` van `java.nio.file` biedt meer controle, zoals het instellen van bestandsattributen.
- In-memory databases of caches kunnen tijdelijke bestanden vervangen voor sommige gebruiksscenario's (zoals `H2` of `Redis`).

Standaard worden tijdelijke bestanden opgeslagen in de standaard tijdelijke bestandsdirectory van het systeem, maar je kunt je eigen pad specificeren. Onthoud dat je jezelf moet opruimen; het is niet gegarandeerd dat tijdelijke bestanden worden verwijderd nadat je programma draait. De `deleteOnExit()` methode zorgt ervoor dat het bestand wordt verwijderd wanneer de JVM wordt afgesloten, maar het is niet waterdicht voor langlopende apps.

## Zie Ook
Meer over tijdelijke bestanden in Kotlin en Java:
- Officiële `File` documentatie van Kotlin: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- Java's `File` klasse: [https://docs.oracle.com/javase/7/docs/api/java/io/File.html](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- Voor een dieper begrip van bestandsattributen: [https://docs.oracle.com/javase/tutorial/essential/io/fileAttr.html](https://docs.oracle.com/javase/tutorial/essential/io/fileAttr.html)
