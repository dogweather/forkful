---
title:    "Kotlin: Entfernung von Zeichen mit einem bestimmten Muster"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum
Manchmal kann es notwendig sein, bestimmte Zeichen oder Muster aus einem Text zu löschen. Dies kann zum Beispiel bei der Datenaufbereitung oder beim Manipulieren von Strings in der Programmierung nützlich sein.

## So geht's
Es gibt verschiedene Möglichkeiten, um in Kotlin Zeichen oder Muster aus einem String zu löschen. Eine einfache Möglichkeit ist die Verwendung der `replace()` Methode. Hier ein Beispielcode:

```Kotlin
val string = "Hello World!"
val newString = string.replace("l", "")
println(newString) // Output: Heo Word!
```

Wie man sehen kann, wird jedes "l" in dem String durch einen leeren String ersetzt, wodurch es effektiv gelöscht wird. Auch die Verwendung von regulären Ausdrücken ist möglich, um komplexere Muster zu löschen. Hier ein Beispielcode:

```Kotlin
val string = "123abc456def"
val newString = string.replace(Regex("[a-z]"), "")
println(newString) // Output: 123456
```

In diesem Fall werden alle Kleinbuchstaben von a bis z durch einen leeren String ersetzt.

## Tiefergehende Informationen
Wenn man sich genauer mit dem Löschen von Zeichen in Kotlin auseinandersetzen möchte, gibt es noch weitere Methoden und Funktionen, die hilfreich sein können. Zum Beispiel kann man auch die `replaceFirst()` Methode verwenden, um nur das erste Vorkommen eines Zeichens zu ersetzen. Außerdem gibt es die Möglichkeit, mithilfe von `toMutableList()` und `removeAll()` alle Zeichen einer Liste zu löschen. Es gibt also verschiedene Ansätze, je nachdem was genau man erreichen möchte.

## Siehe auch
- Offizielle Kotlin-Dokumentation zu `replace()` : https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- Offizielle Kotlin-Dokumentation zu regulären Ausdrücken: https://kotlinlang.org/docs/reference/regular-expressions.html
- Weitere Möglichkeiten, um Zeichen in Strings zu löschen: https://www.baeldung.com/java-remove-character-string