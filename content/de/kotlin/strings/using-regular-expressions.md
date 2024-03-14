---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:22.762608-07:00
description: "Regul\xE4re Ausdr\xFCcke (regex) sind ein m\xE4chtiges Werkzeug zur\
  \ Textverarbeitung, das es Programmierern erm\xF6glicht, mit fortgeschrittenen\u2026"
lastmod: '2024-03-13T22:44:53.837237-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke (regex) sind ein m\xE4chtiges Werkzeug zur Textverarbeitung,\
  \ das es Programmierern erm\xF6glicht, mit fortgeschrittenen\u2026"
title: "Regul\xE4re Ausdr\xFCcke verwenden"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (regex) sind ein mächtiges Werkzeug zur Textverarbeitung, das es Programmierern ermöglicht, mit fortgeschrittenen Musterabgleichtechniken nach Zeichenfolgen zu suchen, Übereinstimmungen zu finden und Zeichenfolgen zu manipulieren. In Kotlin hilft die Nutzung von regex, komplexe Textverarbeitungsaufgaben wie Validierung, Parsing oder Transformation effizient durchzuführen, was es für Aufgaben von einfacher Zeichenfolgenmanipulation bis hin zu komplexer Textanalyse unentbehrlich macht.

## Wie geht das:

### Basisabgleich
Um zu überprüfen, ob eine Zeichenfolge in Kotlin einem bestimmten Muster entspricht, können Sie die `matches` Methode der `Regex` Klasse verwenden.

```kotlin
val pattern = "kotlin".toRegex()
val input = "Ich liebe kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Ausgabe: true
```

### Finden und Extrahieren von Teilen der Zeichenfolge
Wenn Sie Teile einer Zeichenfolge finden möchten, die einem Muster entsprechen, ermöglicht es Kotlin, über alle Übereinstimmungen zu iterieren:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Das heutige Datum ist 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Ausgabe: 07/09/2023
```

### Text ersetzen
Das Ersetzen von Teilen einer Zeichenfolge, die einem Muster entsprechen, ist mit der `replace` Funktion einfach:

```kotlin
val input = "Benutzername: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Ausgabe: Benutzername: userXXX
```

### Zeichenfolgen aufteilen
Teilen Sie eine Zeichenfolge in eine Liste auf, indem Sie ein Regexp-Muster als Trennzeichen verwenden:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Ausgabe: [1, 2, 3, 4, 5]
```

### Drittanbieter-Bibliotheken: Kotest
[Kotest](https://github.com/kotest/kotest) ist eine beliebte Kotlin-Testbibliothek, die die integrierte Regex-Unterstützung von Kotlin erweitert, besonders nützlich für die Validierung in Testfällen.

```kotlin
// Vorausgesetzt, Kotest ist Ihrem Projekt hinzugefügt
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// Dies wird den Test bestehen lassen, wenn die Eingabe dem E-Mail-Muster entspricht.
```

Indem Sie reguläre Ausdrücke in Ihre Kotlin-Anwendungen integrieren, können Sie anspruchsvolle Textverarbeitungsaufgaben effizient durchführen. Ob Sie Benutzereingaben validieren, Daten extrahieren oder Zeichenfolgen transformieren, Regex-Muster bieten eine robuste Lösung.
