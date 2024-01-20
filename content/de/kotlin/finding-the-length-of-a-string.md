---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

In der Programmierung ist die Länge eines Strings die Anzahl der Zeichen in diesem String. Es ist wichtig, diese zu kennen, um Dinge wie Zeichenmanipulationen, Textvalidierung und vieles mehr zu ermöglichen.
  
## Anleitung:

Hier ist ein einfaches Beispiel dafür, wie Du die Länge eines Strings in Kotlin findest:

```Kotlin
fun main() {
  val str = "Hallo Welt"
  println(str.length)
}
```

In diesem Beispiel wird die Ausgabe `11` sein, was der Gesamtzahl der Zeichen im String "Hallo Welt" entspricht.

##Vertiefung:

Die Methode `.length` ist seit Beginn der Sprache in Kotlin integriert, sie wurde von Java übernommen. Sie zählt das Unicode-Zeichen im String, nicht die tatsächlichen Bytes, die den String darstellen. Alternativ könntest Du eine Schleife verwenden, um jedes Zeichen im String manuell zu zählen, aber das wäre ineffizient und nicht empfehlenswert. Die Implementierung von `.length` ist hoch optimiert und bietet die beste Leistung.

## Siehe Auch:

Mehr über String Manipulation in Kotlin, siehe [Kotlin Dokumentation: Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings). 

Für weitere Informationen und Beispiele zur Zeichenkette und ihren Methoden, siehe [Kotlin String API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/).