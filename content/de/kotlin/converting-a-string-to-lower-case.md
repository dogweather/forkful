---
title:    "Kotlin: String in Kleinbuchstaben umwandeln"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Sie fragen sich vielleicht, warum es wichtig ist, eine Zeichenfolge in Kleinbuchstaben umzuwandeln. Es ist wichtig, da dies häufig ein Schritt ist, der bei der Verarbeitung von Benutzereingaben oder beim Vergleichen von Zeichenfolgen verwendet wird.

## Wie geht das?

Die Sprache Kotlin bietet eine einfache und effiziente Möglichkeit, eine Zeichenfolge in Kleinbuchstaben umzuwandeln. Hier sind zwei Beispiele, die Ihnen zeigen, wie es geht:

```Kotlin
val text = "Hallo Welt"
println(text.toLowerCase())
```
Output: hallo welt

```Kotlin
val text = "Life is Amazing"
println(text.toLowerCase())
```
Output: life is amazing

Eine andere Möglichkeit, eine Zeichenfolge in Kleinbuchstaben umzuwandeln, ist die Verwendung der `toLowerCase()` Funktion. Diese Funktion kann auf jede Zeichenfolge angewendet werden und gibt eine neue Zeichenfolge zurück, die in Kleinbuchstaben konvertiert wurde. Zum Beispiel:

```Kotlin
val text = "KOTLIN IST TOLL"
println(text.toLowerCase())
```
Output: kotlin ist toll

## Tiefer in die Materie eintauchen

Wenn Sie sich fragen, wie genau diese Konvertierung von Zeichenfolgen in Kleinbuchstaben funktioniert, gibt es einige wichtige Details zu beachten. Die `toLowerCase()` Funktion verwendet das Standard-Unicode-Zeichensatzformat, um die Zeichen in Kleinbuchstaben zu konvertieren. Dies bedeutet, dass Zeichen mit Akzenten oder anderen diakritischen Zeichen ebenfalls in Kleinbuchstaben umgewandelt werden. Dies kann bei der Verarbeitung von Benutzereingaben oder beim Vergleichen von Zeichenfolgen hilfreich sein, da es die Genauigkeit verbessert.

Eine weitere wichtige Sache zu beachten ist, dass die `toLowerCase()` Funktion die ursprüngliche Zeichenfolge nicht verändert, sondern eine neue Zeichenfolge zurückgibt. Dies ist wichtig, da in Kotlin Strings als unveränderliche Objekte behandelt werden. Daher müssen Sie die Ergebnisse der Funktion einer Variablen zuweisen, wenn Sie das Ergebnis beibehalten möchten.

## Siehe auch

Hier sind einige nützliche Links, um mehr über die Konvertierung von Zeichenfolgen in Kotlin zu erfahren:

- [Offizielle Dokumentation von Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [GeeksforGeeks Tutorial](https://www.geeksforgeeks.org/kotlin-string-tolowercase/)
- [Kotlin Tutorial auf YouTube](https://www.youtube.com/watch?v=IC41pWjU5Lg)