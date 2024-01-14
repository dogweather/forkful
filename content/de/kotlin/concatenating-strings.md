---
title:                "Kotlin: Verkettung von Zeichenfolgen"
simple_title:         "Verkettung von Zeichenfolgen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Situationen in der Programmierung, in denen es notwendig ist, Zeichenketten miteinander zu verbinden. Dies kann zum Beispiel beim Zusammenfügen von Texten, Erstellen von Pfaden oder Generieren von dynamischen HTML-Elementen erforderlich sein.

# Wie man Zeichenketten in Kotlin verbindet

In Kotlin gibt es verschiedene Möglichkeiten, Zeichenketten zu verbinden. Die einfachste Methode ist die Verwendung des Plus-Operators (+), der zwei Zeichenketten zu einer neuen kombiniert.

```Kotlin
val firstName = "Max"
val lastName = "Mustermann"
val fullName = firstName + " " + lastName
println(fullName) // Ausgabe: Max Mustermann
```
Es ist auch möglich, die Methode `plus()` aufzurufen, um Zeichenketten zu verbinden.

```Kotlin
val firstName = "Max"
val lastName = "Mustermann"
val fullName = firstName.plus(" ").plus(lastName)
println(fullName) // Ausgabe: Max Mustermann
```

Eine weitere Option ist die Verwendung von String-Formatierung mit Hilfe von Platzhaltern. Dabei werden die Platzhalter durch die tatsächlichen Werte ersetzt.

```Kotlin
val firstName = "Max"
val lastName = "Mustermann"
val fullName = "%s %s".format(firstName, lastName)
println(fullName) // Ausgabe: Max Mustermann
```

# Tiefergehende Informationen über das Verbinden von Zeichenketten

Es ist wichtig zu beachten, dass das Verbinden von Zeichenketten in Kotlin nicht effizient ist, da dabei immer wieder neue Zeichenketten erzeugt werden müssen. Aus diesem Grund empfiehlt es sich, wenn möglich, die `StringBuilder`-Klasse zu verwenden.

Die `StringBuilder`-Klasse ermöglicht es, Zeichenketten effizient zu verbinden, da dabei nur ein Objekt erstellt wird und die internen Zeichenketten angepasst werden können.

```Kotlin
val firstName = "Max"
val lastName = "Mustermann"
val fullNameBuilder = StringBuilder(firstName)
fullNameBuilder.append(" ")
fullNameBuilder.append(lastName)
println(fullNameBuilder.toString()) // Ausgabe: Max Mustermann
```

Außerdem ist es möglich, die Methode `joinToString()` auf einer Collection aufzurufen, um die Elemente mit einem bestimmten Trennzeichen zu verbinden.

```Kotlin
val numbers = listOf(1, 2, 3, 4, 5)
val numberString = numbers.joinToString(", ")
println(numberString) // Ausgabe: 1, 2, 3, 4, 5
```

Es ist auch wichtig zu beachten, dass in Kotlin Zeichenketten im Gegensatz zu anderen Datentypen wie Integers oder Floats unveränderbar sind. Das bedeutet, dass jede Veränderung an einer Zeichenkette eine neue Zeichenkette erzeugt. Deshalb ist es sinnvoll, die entsprechenden Methoden auf der ursprünglichen Zeichenkette aufzurufen, anstatt eine neue Zeichenkette zu erstellen.

# Siehe auch

- [Offizielle Dokumentation von JetBrains: String Concatenation](https://kotlinlang.org/docs/reference/basic-types.html#string-concatenation)
- [Kotlin String Formatting Guide](https://kotlinlang.org/docs/reference/basic-types.html#string-formatting)
- [Effizientes Verbinden von Zeichenketten in Kotlin](https://blog.kotlin-academy.com/string-concatenation-and-joining-kotlin-performance-pitfalls-f3aa5e01e55b)