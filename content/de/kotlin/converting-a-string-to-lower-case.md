---
title:    "Kotlin: Umwandlung eines Strings in Kleinbuchstaben"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Strings in Kleinbuchstaben kann in der Programmierung hilfreich sein, um die Konsistenz in der Verarbeitung von Benutzereingaben sicherzustellen oder bei der Vergleichung von Strings.

## Wie man es macht

Eine Möglichkeit, einen String in Kleinbuchstaben zu konvertieren, ist die Verwendung der `lowercase()` Funktion in Kotlin. Hier ist ein Beispielcode:

```Kotlin
val eingabe = "HALLO WELT"
val konvertiert = eingabe.lowercase()

println(konvertiert)
// Output: hallo welt
```

Wie man sieht, wird der String "HALLO WELT" in der Variable `konvertiert` in Kleinbuchstaben umgewandelt. Dies kann auch direkt beim Aufrufen der `lowercase()` Funktion gemacht werden:

```Kotlin
val eingabe = "HALLO WELT"

println(eingabe.lowercase())
// Output: hallo welt
```

Es ist auch möglich, nur einen Teil eines Strings in Kleinbuchstaben zu konvertieren, indem man einen Bereich der `lowercase()` Funktion übergibt. Hier ist ein Beispiel:

```Kotlin
val eingabe = "Hallo Welt"

println(eingabe.lowercase(range = 6..8))
// Output: halLO Welt
```

In diesem Beispiel wird der Buchstabenbereich von Position 6 bis 8 (inklusive) in Kleinbuchstaben konvertiert.

## Tiefergehende Informationen

Bei der Umwandlung von Strings in Kotlin wird standardmäßig der Unicode-Buchstabensatz verwendet. Dies bedeutet, dass auch Buchstaben mit Akzenten oder Sonderzeichen in das entsprechende Äquivalent in Kleinbuchstaben umgewandelt werden.

Es ist auch wichtig zu beachten, dass durch die Konvertierung eines Strings in Kleinbuchstaben das ursprüngliche Objekt nicht verändert wird. Stattdessen wird eine neue String-Instanz mit dem konvertierten Text erstellt.

Hier sind einige weitere Funktionen in Kotlin, die beim Konvertieren von Strings hilfreich sein können:

- `uppercase()` - konvertiert einen String in Großbuchstaben
- `capitalize()` - konvertiert den ersten Buchstaben eines Strings in einen Großbuchstaben
- `reversed()` - dreht die Reihenfolge der Zeichen in einem String um

## Siehe auch

- Kotlin String-Dokumentation: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/
- Guide zur Verwendung von Strings in Kotlin: https://www.baeldung.com/kotlin/strings