---
title:                "Kotlin: Umwandeln einer Zeichenkette in Kleinbuchstaben"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Strings in Kleinbuchstaben ist häufig eine wichtige Aufgabe in der Programmierung. Es kann verwendet werden, um die Benutzereingabe zu vereinheitlichen, die String-Vergleiche zu erleichtern oder einfaches Textformatieren zu ermöglichen.

## Wie es gemacht wird

Um einen String in Kotlin in Kleinbuchstaben umzuwandeln, verwenden wir die Methode `toLowerCase()` und weisen den zurückgegebenen Wert einer anderen Variablen zu. Hier ist ein Beispielcode:

```Kotlin
val original = "Hey Du!"
val lowerCase = original.toLowerCase()

println("$original -> $lowerCase")
```

Die Ausgabe dieses Codes ist `Hey Du! -> hey du!`. Wir können auch direkt auf den String `toLowerCase()` anwenden, ohne die ursprüngliche Variable zu ändern:

```Kotlin
val upperCase = "YOLO".toLowerCase()

println("$upperCase -> YOLO")
```

Die Ausgabe ist `yolo -> YOLO`.

Wir können auch die Methode `toLowerCase(Locale)` verwenden, um die Locale des Geräts zu berücksichtigen. Dies ist hilfreich, wenn wir eine korrekte Groß- und Kleinschreibung in verschiedenen Sprachen sicherstellen möchten.

## Tiefer eintauchen

Bei der Konvertierung von Strings in Kotlin sollten wir auch die Verwendung von String-Erweiterungsfunktionen erwähnen. Diese können auf Strings angewendet werden, ohne eine zusätzliche Variable zu erstellen.

Zum Beispiel erweitert `toLowerCase()` automatisch die Klasse `String` und kann direkt auf einer Zeichenfolge aufgerufen werden:

```Kotlin
println("HELLO".toLowerCase()) // Ausgabe: hello
```

Diese Erweiterungsfunktionen sind auch in Bibliotheken wie Kotlin's `stdlib` verfügbar.

Es ist auch erwähnenswert, dass die in der Standardbibliothek bereitgestellte `toLowerCase()`-Methode Unicode-tauglich ist und somit auch für Sonderzeichen verwendet werden kann.

## Siehe auch

- [Kotlin String Documenation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [String.toLowerCase() in Java](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
- [How to Convert Strings to Lowercase in Kotlin](https://www.baeldung.com/kotlin/string-to-lowercase)