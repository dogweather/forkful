---
date: 2024-01-20 17:38:48.321835-07:00
description: "Umwandlung einer Zeichenkette in Kleinbuchstaben bedeutet, dass alle\
  \ Gro\xDFbuchstaben in ihrer kleingeschriebenen Form dargestellt werden. Das ist\
  \ n\xFCtzlich\u2026"
lastmod: '2024-03-13T22:44:53.834467-06:00'
model: gpt-4-1106-preview
summary: "Umwandlung einer Zeichenkette in Kleinbuchstaben bedeutet, dass alle Gro\xDF\
  buchstaben in ihrer kleingeschriebenen Form dargestellt werden."
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## Was & Warum?
Umwandlung einer Zeichenkette in Kleinbuchstaben bedeutet, dass alle Großbuchstaben in ihrer kleingeschriebenen Form dargestellt werden. Das ist nützlich für Konsistenz bei Vergleichen oder wenn man Eingaben normalisieren will.

## Anleitung:
In Kotlin ist die Umwandlung simpel — nutze die `toLowerCase()` Methode. Hier ist ein Beispiel:

```kotlin
fun main() {
    val text = "Kotlin IST Großartig!"
    val lowerCaseText = text.lowercase()
    println(lowerCaseText)
}
```
Ausgabe:
```
kotlin ist großartig!
```

## Tiefgang:
Das Umwandeln in Kleinbuchstaben ist nicht neu und kommt schon lange in vielen Programmiersprachen vor. In Kotlin ersetzt `lowercase()` die ältere `toLowerCase()` Methode und behandelt standardmäßig auch Sonderzeichen richtig. Es gibt die Möglichkeit, mittels `Locale` die Umwandlung regionsspezifisch zu steuern. Beispielsweise unterscheidet sich das türkische `i` ohne Punkt vom englischen `i`.

Alternativen? Manchmal ist `capitalize()` oder `toUpperCase()` gefragt, je nachdem, was mit der Zeichenkette passieren soll.

Unter der Haube verwendet Kotlin Funktionen von der Java `String` Klasse, die wiederum auf der Unicode-Standardisierung basieren, um sicherzustellen, dass die Konversion von Groß- zu Kleinbuchstaben gemäß den Regeln für die entsprechende Sprache funktioniert.

## Siehe Auch:
- Kotlin Dokumentation zur `lowercase()` Methode: [Kotlin String.lowercase](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- Unicode Standard für Groß- und Kleinschreibung: [Unicode Case Folding](https://www.unicode.org/reports/tr21/tr21-5.html)
