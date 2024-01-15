---
title:                "Löschen von Zeichen mit entsprechendem Muster"
html_title:           "Kotlin: Löschen von Zeichen mit entsprechendem Muster"
simple_title:         "Löschen von Zeichen mit entsprechendem Muster"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Manchmal suchen Programmierer nach einer Möglichkeit, bestimmte Zeichen in einem Text zu löschen, die einem bestimmten Muster entsprechen. Dies kann eine hilfreiche Funktion sein, um unerwünschte Zeichen zu entfernen und den Text zu bereinigen.

## Wie geht das

Um Zeichen zu löschen, die einem Muster entsprechen, kann die Funktion `replace()` verwendet werden. Diese nimmt zwei Parameter an, das Muster als regulärer Ausdruck und den zu ersetzenden Text. Hier ist ein Beispielcode:

```Kotlin
val text = "Dies ist ein Beispieltext mit unerwünschten Zeichen: äüöß"
val result = text.replace("[äöüß]".toRegex(), "")

println(result)
```

Die Ausgabe des Codes wird sein:

> Dies ist ein Beispieltext mit unerwünschten Zeichen:

Das Muster `"[äöüß]"` entspricht allen Vorkommen der Zeichen ä, ö, ü und ß, die dann durch einen leeren String ersetzt werden.

Natürlich können auch komplexere Muster verwendet werden, um spezifischere Zeichen zu löschen.

## Tiefergehend

Die Funktion `replace()` verwendet reguläre Ausdrücke, um das Muster zu definieren. Reguläre Ausdrücke sind eine leistungsstarke Sprache, um Muster in Texten zu erkennen und zu manipulieren. Eine detaillierte Erklärung aller regulären Ausdrücke würde den Rahmen dieses Artikels sprengen, aber hier sind einige Beispiele, die beim Löschen von Zeichen hilfreich sein können:

- `"[0-9]"` entspricht allen Ziffern von 0 bis 9.
- `"[^a-zA-Z]"` entspricht allen Zeichen außer den Buchstaben a-z und A-Z.
- `"\s"` entspricht allen Leerzeichen, Tabulatoren oder Zeilenumbruchzeichen.
- `"[^\w]"` entspricht allen Nicht-Buchstaben, Nicht-Ziffern und Nicht-Unterstrichen.

Es gibt viele weitere reguläre Ausdrücke, die für verschiedene Situationen nützlich sein können. Es ist empfehlenswert, sich gründlich mit ihnen vertraut zu machen, um ihren vollen Nutzen zu verstehen.

## Siehe Auch

- Offizielle Kotlin-Dokumentation zur `replace()`-Funktion: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- Reguläre Ausdrücke in Kotlin: https://kotlinlang.org/docs/regular-expressions.html
- Tutorial zu regulären Ausdrücken von Codecademy (auf Englisch): https://www.codecademy.com/learn/learn-regular-expressions