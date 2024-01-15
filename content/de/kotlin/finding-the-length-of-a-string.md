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

## Warum
Es gibt viele Anwendungen, bei denen man die Länge eines Strings benötigt, z.B. zur Validierung von Benutzereingaben oder zur Erstellung von benutzerdefinierten Funktionen. Mit Kotlin ist es ganz einfach, die Länge eines Strings zu ermitteln und so das volle Potenzial der Sprache auszuschöpfen.

## Wie geht das?
Das Ermitteln der Länge eines Strings ist mit Kotlin sehr einfach. Man muss nur die `length`-Eigenschaft des Strings aufrufen, um die Anzahl der Zeichen zu erhalten. Hier ist ein Beispielcode, um die Länge eines Strings zu bestimmen:

```Kotlin
val str = "Hallo Welt"
println(str.length)
```

Die Ausgabe dieses Codeschnipsels wäre `11`, da der String "Hallo Welt" aus 11 Zeichen besteht. Man kann auch direkt die Länge in einer Variable speichern, um sie später weiterzuverwenden:

```Kotlin
val str = "Dies ist ein langer Satz"
val length = str.length
println("Der Satz enthält $length Zeichen.")
```

Die Ausgabe dieses Codeschnipsels wäre `Der Satz enthält 24 Zeichen.`. So kann man die Länge eines Strings auf vielfältige Weise in seinem Code nutzen.

## Tiefergehende Informationen
Die `length`-Eigenschaft gibt die Anzahl der Zeichen eines Strings zurück, unabhängig von der Art der Zeichen. Das bedeutet, dass auch Leerzeichen, Sonderzeichen und Umlaute in die Länge mit einbezogen werden. Wenn man nur die Anzahl der Buchstaben eines Strings ermitteln möchte, gibt es noch die `count()`-Funktion. Diese gibt die Anzahl der Zeichen zurück, die dem angegebenen Kriterium entsprechen. Hier ein Beispiel, um nur die Anzahl der Buchstaben zu erhalten:

```Kotlin
val str = "Hallo Welt"
val letters = str.count { it.isLetter() }
println("Der String enthält $letters Buchstaben.")
```

Die Ausgabe wäre in diesem Fall `Der String enthält 9 Buchstaben.`. Die `isLetter()`-Funktion überprüft dabei, ob es sich bei dem Zeichen um einen Buchstaben handelt.

## Siehe auch
- [Kotlin String Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin Collection Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/)