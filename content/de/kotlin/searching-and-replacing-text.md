---
title:                "Kotlin: Suchen und Ersetzen von Text."
simple_title:         "Suchen und Ersetzen von Text."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Ersetzen von Text ist eine häufige Aufgabe in der Programmierung. Wenn du schnell und effizient Text in deinem Code ändern möchtest, ist es wichtig zu wissen, wie man gezielt sucht und ersetzt.

## Wie geht das?

Die Kotlin Standardbibliothek bietet verschiedene Funktionen für das Suchen und Ersetzen von Text. Hier sind einige Beispiele:

```
val text = "Hallo Welt"
val newText = text.replace("Welt", "Kotlin")
println(newText) // Output: Hallo Kotlin
```

In diesem Beispiel wird die Funktion `replace()` verwendet, um das Wort "Welt" durch "Kotlin" zu ersetzen. Du kannst auch reguläre Ausdrücke verwenden, um gezielter zu suchen und zu ersetzen:

```
val text = "Dies ist eine Beispielstrecke mit verschiedenen Stichwörtern"
val newText = text.replace(Regex("Stichwörter"), "Schlagworte")
println(newText) // Output: Dies ist eine Beispielstrecke mit verschiedenen Schlagworte
```

Du kannst auch die Funktion `replaceFirst()` verwenden, um nur das erste Vorkommen eines bestimmten Textes zu ersetzen, oder `replaceBefore()` und `replaceAfter()`, um Text vor oder nach einem bestimmten Text zu ersetzen. Die Kotlin Standardbibliothek bietet noch viele weitere Funktionen für das Suchen und Ersetzen von Text.

## Tiefer Einblick

Wie du siehst, gibt es viele Möglichkeiten, Text in Kotlin zu suchen und zu ersetzen. Es ist jedoch wichtig zu beachten, dass diese Funktionen Strings nicht direkt ändern, sondern eine kopierte Version des Strings mit den gewünschten Änderungen zurückgeben. Wenn du also eine große Anzahl an String-Operationen durchführst, solltest du stattdessen die `StringBuilder` Klasse verwenden, um die Leistung zu verbessern.

Eine weitere wichtige Sache bei der Textersetzung ist die Verwendung von regulären Ausdrücken, um gezielt zu suchen und ein breiteres Spektrum an Textmustern abzudecken. Es kann hilfreich sein, sich mit regulären Ausdrücken vertraut zu machen, um effektiv Text in deinem Code zu manipulieren.

## Siehe auch

- [Offizielle Dokumentation von Kotlin zu String-Ersetzungen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Reguläre Ausdrücke in Kotlin](https://kotlinlang.org/docs/reference/regular-expressions.html)