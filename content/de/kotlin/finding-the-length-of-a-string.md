---
title:    "Kotlin: Die Länge eines Strings finden"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Als Entwickler oder Entwicklerin in Kotlin ist es wichtig zu wissen, wie man die Länge von Strings findet. Dies ist eine grundlegende Fähigkeit, die in vielen Programmen und Anwendungen nützlich ist.

## Wie man die Länge eines Strings findet

Um die Länge eines Strings zu finden, gibt es in Kotlin eine einfache Methode. Schauen wir uns ein Beispiel an:

```Kotlin
val string = "Hallo Welt!"
val length = string.length
println(length)
```

Dieses kurze Beispiel zeigt, wie die "length" Methode auf einem String angewendet werden kann. Die Ausgabe wird "11" sein, da das Leerzeichen auch als Zeichen zählt.

Eine weitere Möglichkeit, die Länge eines Strings zu finden, wäre die Nutzung von String-Interpolation. Dies bedeutet, dass man den String in eine andere Variable packt und dann die "length" Methode darauf anwendet. Ein Beispiel dafür wäre:

```Kotlin
val string = "Guten Morgen!"
val length = "${string.length}"
println("Die Länge des Strings ist $length")
```

Hier wird der Wert der "length" Methode direkt in die Ausgabe eingefügt.

## Eintauchen in Strings

Es gibt noch einige weitere interessante Informationen über Strings und ihre Länge. Zum Beispiel gibt es in Kotlin auch die Möglichkeit, mit Multiline-Strings zu arbeiten. Dies sind Strings, die über mehrere Zeilen gehen und oft für längere Textpassagen verwendet werden.

Das Besondere an diesen Multiline-Strings ist, dass sie nicht die "length" Methode verwenden können, um ihre Länge zu finden. Stattdessen muss die "lines" Methode verwendet werden, um die Anzahl der Zeilen zu zählen. Ein Beispiel dazu wäre:

```Kotlin
val multilineString = """
    Dies ist ein Multiline-String
    mit mehreren Zeilen
    und sogar Leerzeichen am Anfang und Ende.
""".trimIndent()

val numberOfLines = multilineString.lines().size
println(numberOfLines)
```

Dieses Beispiel zeigt, wie man die Anzahl der Zeilen in einem Multiline-String finden kann, indem man die "lines" Methode verwendet. Die Ausgabe wird "3" sein, da der String aus drei Zeilen besteht.

## Siehe auch

Für weitere Informationen über die Arbeit mit Strings und deren Länge in Kotlin, schau dir diese nützlichen Ressourcen an:

- [Kotlin Dokumentation: Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutorial: Working with Strings in Kotlin](https://www.baeldung.com/kotlin/strings)
- [String Interpolation in Kotlin](https://www.geeksforgeeks.org/interpolation-string-in-kotlin/)

Wir hoffen, dass dieser Artikel dir geholfen hat, die Länge von Strings in Kotlin besser zu verstehen und dir bei zukünftigen Projekten von Nutzen sein wird. Happy coding!