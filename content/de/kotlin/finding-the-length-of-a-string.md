---
title:                "Kotlin: Das Finden der Länge eines Strings."
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Länge einer Zeichenfolge zu finden, ist eine häufige Aufgabe beim Programmieren. Es ist wichtig, die Länge einer Zeichenfolge zu kennen, um effizient mit ihr arbeiten zu können. In diesem Artikel werden wir uns ansehen, wie man die Länge einer Zeichenfolge in Kotlin finden kann.

## Wie geht's?

Um die Länge einer Zeichenfolge in Kotlin zu finden, können wir die ```length```-Funktion verwenden. Diese Funktion gibt uns die Anzahl der Zeichen in der Zeichenfolge zurück. Schauen wir uns dazu ein Beispiel an:

```Kotlin
val string = "Hallo Welt"
println(string.length)
```

Das obige Beispiel gibt die Zahl 10 aus, da die Zeichenfolge "Hallo Welt" 10 Zeichen lang ist.

Eine weitere Möglichkeit, die Länge einer Zeichenfolge zu finden, ist die Verwendung der ```count```-Funktion. Diese Funktion zählt die Anzahl der Elemente in einer Kollektion, was auch für Zeichenfolgen gilt. Hier ist ein Beispiel:

```Kotlin
val string = "Hey"
println(string.count())
```

Dieses Beispiel gibt die Zahl 3 aus, da die Zeichenfolge "Hey" aus 3 Buchstaben besteht.

## Tiefentauchen

In Kotlin gibt es verschiedene Möglichkeiten, die Länge einer Zeichenfolge zu finden. Neben den oben genannten Funktionen gibt es auch noch die Funktion ```size```, die uns die Größe einer Zeichenfolge zurückgibt. Diese Funktion ist besonders nützlich, wenn wir mit Arrays oder anderen Datenstrukturen arbeiten. Hier ein Beispiel:

```Kotlin
val string = "Hallo"
println(string.size)
```

Dieses Beispiel gibt die Zahl 5 aus, da die Größe der Zeichenfolge "Hallo" 5 ist.

Zusätzlich gibt es auch noch die Funktion ```length()```, die das gleiche wie ```length``` tut. Sie kann jedoch auf Java-basierten Datenstrukturen verwendet werden. Hier ein Beispiel:

```Kotlin
val list = arrayListOf("Eins", "Zwei", "Drei")
println(list[2].length())
```

Dieses Beispiel gibt die Zahl 4 aus, da das Wort "Drei" 4 Zeichen hat.

## Siehe auch

- Kotlin offizielle Dokumentation: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Erklärvideo zur Länge von Zeichenfolgen in Kotlin: https://www.youtube.com/watch?v=5SulWAsf0Aw
- Weitere nützliche Kotlin-Programmiertricks: https://blog.mindorks.com/5-useful-kotlin-tricks-for-android-development