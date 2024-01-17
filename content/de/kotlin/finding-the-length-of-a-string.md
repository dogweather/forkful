---
title:                "Die Länge eines Strings finden"
html_title:           "Kotlin: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge einer Zeichenfolge ist die Anzahl der Zeichen in dieser Zeichenfolge. Programmierer verwenden oft die Länge einer Zeichenfolge, um verschiedene Operationen durchzuführen, wie zum Beispiel das Validieren von Nutzereingaben oder das Überprüfen von Bedingungen in Schleifen.
## Wie:
```Kotlin
val string = "Hallo!"
println(string.length)
```
Output:
>>> 6

## Tiefergehende Infos:
Die Methode, um die Länge einer Zeichenfolge zu finden, ist seit den Anfängen der Programmierung weit verbreitet. Sie ist sehr nützlich, um mit nutzergenerierten Daten umzugehen. Eine Alternative zur Längenbestimmung einer Zeichenfolge ist die Verwendung einer for-Schleife mit einem Zähler, der jedes Zeichen durchläuft und jeweils um 1 erhöht wird. In Kotlin ist die Länge einer Zeichenfolge das Ergebnis der Aufrufs der Methode "length".

## Siehe auch:
- [Kotlin String API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [How to Get the Length of a String in Kotlin](https://www.baeldung.com/kotlin/string-length)