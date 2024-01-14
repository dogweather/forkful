---
title:                "Kotlin: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum

Das Extrahieren von Teilzeichenketten, auch Substrings genannt, ist eine häufige Aufgabe beim Programmieren. Oftmals müssen Daten aus größeren Zeichenketten herausgefiltert werden, um sie weiterzuverarbeiten oder auszugeben. In diesem Artikel werden wir uns genauer ansehen, wie man in Kotlin Substrings extrahiert und welche Methoden dafür zur Verfügung stehen.

## Wie geht man vor

Um in Kotlin Substrings zu extrahieren, gibt es zwei Hauptmethoden: `substring()` und `slice()`. Die `substring()` Methode extrahiert einen Teil der Zeichenkette basierend auf einem angegebenen Start- und Endindex. Zum Beispiel:

```Kotlin
val str = "Hallo Welt"
val substr = str.substring(0, 5)

println(substr) // gibt "Hallo" aus
```

Hier haben wir einen String `str` erstellt und dann mit der `substring()` Methode einen Substring `substr` extrahiert, der aus den ersten fünf Zeichen des ursprünglichen Strings besteht.

Die `slice()` Methode ermöglicht es, Substrings basierend auf einer Liste von angegebenen Indizes zu extrahieren. Zum Beispiel:

```Kotlin
val str = "Hallo Welt"
val indices = listOf(0, 6, 10)

val substr = str.slice(indices)

println(substr) // gibt "Hallo" aus
```

In diesem Beispiel haben wir eine Liste von Indizes erstellt, die die Positionen der Buchstaben "H", "W" und "t" im String `str` angeben. Mit der `slice()` Methode extrahieren wir dann den Substring, der aus diesen Buchstaben besteht.

## Tiefer gehende Einblicke

Beim Extrahieren von Substrings ist es wichtig zu wissen, wie Indizes in Strings gezählt werden. In Kotlin und den meisten anderen Programmiersprachen werden die Indizes von 0 aus gezählt, was bedeutet, dass das erste Zeichen in einer Zeichenkette den Index 0 hat. Auch bei der `substring()` Methode gibt der Endindex nicht den tatsächlichen Buchstaben an der Position an, sondern die Position des nächsten Buchstabens. Zum Beispiel bedeutet `substring(0, 5)` im obigen Beispiel, dass der Substring von Index 0 bis 4 (nicht 5) extrahiert wird. Dies ist wichtig zu beachten, um sicherzustellen, dass der gewünschte Substring korrekt extrahiert wird.

Es ist auch möglich, eine negative Zahl als Index zu verwenden, um von hinten zu zählen. Zum Beispiel würde `str.substring(6, -2)` den Substring von Index 6 bis 9 (nicht -2) extrahieren, was den Buchstaben "e" aus "Welt" extrahieren würde.

## Siehe auch

Für weitere Informationen und Beispiele zum Extrahieren von Substrings in Kotlin, können folgende Links hilfreich sein:

- [Kotlin Dokumentation zu Substrings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [GeeksforGeeks Artikel zu Substrings in Kotlin](https://www.geeksforgeeks.org/kotlinjdk-strings-substring-int-int-method/#:~:text=In%20Kotlin%2C%20the%20substring()%20function,substring%20is%20being%20extracted%20is)
- [YouTube Tutorial zu Substrings in Kotlin](https://www.youtube.com/watch?v=9_3OZHtLyxk)