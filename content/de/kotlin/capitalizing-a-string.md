---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
simple_title:         "String in Großbuchstaben umwandeln"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings bedeutet, alle Buchstaben in Großbuchstaben umzuwandeln. Programmierer nutzen dies, um Text konsistent zu gestalten oder um Schlüsselwörter hervorzuheben.

## Wie geht das?
```Kotlin
fun main() {
    val kleinerText = "kotlin ist spaß"
    val großerText = kleinerText.uppercase()

    println(großerText)  // KOTLIN IST SPAß
}
```

## Deep Dive
Großschreibung war schon in frühen Computersystemen wichtig, um Befehle oder Konstanten hervorzuheben. In Kotlin verwendet `uppercase()`, um einen String zu kapitalisieren. Früher gab es `toUpperCase()`, aber seit Kotlin 1.5 ist `uppercase()` die bevorzugte Option. Es befolgt Unicode-Standards und ist damit international einsetzbar. Zusätzlich gibt es auch `capitalize()`, welches nur den ersten Buchstaben eines Strings in einen Großbuchstaben umwandelt. Dies wurde jedoch seit Kotlin 1.5 als veraltet markiert und durch `replaceFirstChar` ersetzt, sollte man dies individuell anwenden wollen.

Alternativen:
- Man kann `map` und `joinToString` nutzen, um eigene Konvertierungsfunktionen zu schreiben. Nicht benötigt für einfache Kapitalisierung, aber nützlich für spezifische Anpassungen.
- Bibliotheken wie Apache Commons bieten `StringUtils.capitalize` an, falls man in einem umfangreichen Projekt arbeitet, wo diese Bibliotheken bereits im Einsatz sind.

Implementierungsdetails:
- `uppercase()` nutzt intern die Funktion `toUpperCase` des Java `String` Objekts, kümmert sich jedoch um spezielle Fälle, wie es Unicode-Standard erfordert.
- In Performance-kritischen Anwendungen sollte man sich bewusst sein, dass Großschreibung eine neue String-Instanz erzeugt und die Original-Instanz unverändert lässt. 

## Siehe auch
- [Kotlin Dokumentation zu upperCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/uppercase.html)
- [Unicode Standard für Groß- und Kleinschreibung](https://unicode.org/reports/tr21/tr21-5.html)
- [Apache Commons StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
