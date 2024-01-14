---
title:    "Kotlin: Unterstrings extrahieren"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum substrings in Kotlin extrahieren?

Substrings sind Teilstrings eines größeren Strings, die anhand eines Index oder einer bestimmten Länge ausgewählt werden können. In der Programmierung können substrings sehr nützlich sein, insbesondere in der Textverarbeitung. Mit Kotlin können substrings einfach und effizient extrahiert werden, was die Arbeit mit Strings erleichtert.

## Wie man Substrings in Kotlin extrahiert

Das Extrahieren von Substrings in Kotlin ist sehr einfach und erfordert nur wenige Zeilen Code. Hier ist ein Beispiel, wie man einen Substring anhand eines Index extrahiert:

```Kotlin
val text = "Hallo, Welt!"
val substring = text.substring(7)
println(substring)
```

Dieses Beispiel würde den Substring "Welt!" extrahieren und ausgeben.

Ebenso kann man auch anhand einer bestimmten Länge einen Substring extrahieren, indem man einen zusätzlichen Parameter in die `substring()` Methode einfügt:

```Kotlin
val text = "Der Schnellbrauebrauer braut an die Schnellbrauebraue"
val substring = text.substring(4, 18)
println(substring)
```

Dieser Code würde den Substring "Schnellbrauebrauer" ausgeben.

## Tiefere Einblicke

Die `substring()` Methode in Kotlin verwendet den Anfangsindex und den Endindex, um den Substring zu extrahieren. Es ist wichtig zu beachten, dass der Endindex nicht zum Substring gehört, sondern exklusiv ist. Das bedeutet, dass der Endindex nicht Teil des Substrings ist, sondern der Index direkt nach dem letzten Zeichen.

Außerdem sollten Entwickler darauf achten, dass der Endindex innerhalb des Textes liegt, da sonst eine `IndexOutOfBoundsException` ausgelöst wird.

## Siehe auch

- [Kotlin Strings Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/kotlin.-string/index.html)
- [Java String Methoden](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html) (Kotlin basiert auf Java und verwendet viele ähnliche Methoden, einschließlich `substring()`)
- [Tutorial: Textverarbeitung in Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_string_processing.htm)

Vielen Dank fürs Lesen! Wir hoffen, dieser Artikel hat Ihnen geholfen, mehr über die Extraktion von substrings in Kotlin zu erfahren. Happy Coding!