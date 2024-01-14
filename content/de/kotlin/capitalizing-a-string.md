---
title:    "Kotlin: String in Großbuchstaben umwandeln"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Das Übergeben von Strings an unsere Programme und Apps ist eine alltägliche Aufgabe, die wir alle tun müssen. Oftmals müssen wir diese Strings auf bestimmte Weise formatieren, um unsere Anforderungen zu erfüllen. Eines der häufigsten Formatierungsanforderungen ist das Großschreiben eines Strings.

## Wie man Strings in Kotlin großschreibt

Die Kotlin Standardbibliothek bietet eine nützliche Methode namens `toUpperCase()`, mit der wir einen String in Großbuchstaben umwandeln können. Hier ist ein Beispiel:

```Kotlin
val string = "hallo welt"
val capitalizedString = string.toUpperCase()
println(capitalizedString)
//Output: HALLO WELT
```

Wir können auch einen Parameter an `toUpperCase()` übergeben, um die Großschreibung abhängig von der verwendeten Lokalisierung anzupassen. Zum Beispiel:

```Kotlin
val string = "hallo welt"
val locale = Locale("de", "DE")  //lokale für deutsch (Deutschland)
val capitalizedString = string.toUpperCase(locale)
println(capitalizedString)
//Output: HALLO WELT
```

Wie Sie sehen, wurde der String entsprechend der deutschen Großschreiberegeln formatiert.

## Tiefere Einblicke in das Großschreiben von Strings

Die `toUpperCase()` Methode verwendet die Unicode-Tabelle, um jeden Buchstaben im String in den entsprechenden Großbuchstaben umzuwandeln. Dies bedeutet, dass es internationalisierbare Ergebnisse liefert und nicht nur für die englische Sprache geeignet ist.

Darüber hinaus arbeitet die Methode nicht-destruktiv, was bedeutet, dass sie den ursprünglichen String nicht verändert, sondern eine neue kopierte Version des Strings zurückgibt.

Ein wichtiger Punkt zu beachten ist, dass `toUpperCase()` nicht nur Buchstaben konvertiert, sondern auch Sonderzeichen, Zahlen und Leerzeichen unverändert lässt.

## Siehe auch

- [Kotlin-Dokumentation zu String-Methoden](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Tutorial zu Unicode und Strings in Kotlin](https://developer.android.com/codelabs/basic-android-kotlin-training-strings/index.html)
- [Kotlin-Konvertierung eines Strings in Großbuchstaben](https://www.baeldung.com/java-string-to-uppercase)