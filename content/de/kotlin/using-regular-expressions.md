---
title:                "Kotlin: Verwendung von regulären Ausdrücken"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum
Viele Entwickler verwenden reguläre Ausdrücke, um Textmuster zu erkennen und zu manipulieren. In Kotlin haben wir die `Regex`-Klasse, die uns dabei hilft.

## Wie man es benutzt
Um eine reguläre Ausdruck in Kotlin einzusetzen, müssen wir zuerst ein `Regex`-Objekt erstellen und dann die Funktion `find()` aufrufen, um das erste Vorkommen des Musters in einem String zu finden. Wir können auch die Funktionen `matches()` und `replace()` verwenden, um das Vorkommen des Musters zu überprüfen bzw. zu ersetzen.

Hier ist ein Beispiel, das die Basics zeigt:

```Kotlin
val regex = Regex("Hallo")
val text = "Hallo Welt!"
if (regex.matches(text)) {
    println("Der Text enthält das Wort 'Hallo'")
}

val result = regex.replace(text, "Hi")
println(result) // Output: Hi Welt!
```

In diesem Beispiel verwenden wir die vordefinierte Funktion `Regex()` anstatt einen regulären Ausdruck als String zu erstellen. Dies kann hilfreich sein, wenn wir variable Teile des Musters haben oder die Flags zum Suchen wie "ignore case" festlegen möchten.

## Tiefer Einblick
Die `Regex`-Klasse in Kotlin ist in der Lage, komplexe Muster zu verarbeiten, die aus speziellen Zeichen, Metazeichen und Quantoren bestehen. Hier sind einige der gebräuchlichsten davon:

- `.` : ein beliebiges Zeichen außer einer Zeilenumbruchsequenz
- `*` : 0 oder mehr Vorkommen des vorherigen Musters
- `+` : 1 oder mehr Vorkommen des vorherigen Musters
- `?` : 0 oder 1 Vorkommern des vorherigen Musters
- `^` : Match am Anfang des Strings
- `$` : Match am Ende des Strings
- `[]` : Liste von Zeichen, die gematcht werden sollen
- `()` : Gruppieren von Teilmustern

Ein guter Weg, um reguläre Ausdrücke in Kotlin zu lernen, ist die offizielle [Dokumentation](https://kotlinlang.org/docs/regular-expressions.html) von Kotlin oder [RegExr](https://regexr.com/), eine Online-Plattform für interaktive reguläre Ausdrücke.

## Siehe auch
- [Offizielle Dokumentation von Kotlin zu regulären Ausdrücken](https://kotlinlang.org/docs/regular-expressions.html)
- [RegExr - Interaktive Plattform für reguläre Ausdrücke](https://regexr.com/)
- [Reguläre Ausdrücke in Java und Kotlin](https://www.baeldung.com/java-regex-kotlin)