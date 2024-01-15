---
title:                "Zeichenketten verbinden"
html_title:           "Kotlin: Zeichenketten verbinden"
simple_title:         "Zeichenketten verbinden"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man überhaupt Strings miteinander verbinden? Nun, das ist ziemlich einfach - String Concatenation ist eine grundlegende Operation der Programmierung, die es ermöglicht, bestehende Strings zu verbinden, um neue Textblöcke zu erstellen. Dies ist besonders nützlich, um dynamische Ausgaben zu erzeugen, die sich je nach Benutzereingabe oder anderen Variablen ändern können.

## Wie geht das?

Die Verkettung von Strings in Kotlin ist relativ einfach. Durch die Verwendung vom "plus" Operator (+) können Strings einfach mit anderen Werten oder Variablen verbunden werden. Hier ist ein einfaches Beispiel:

```Kotlin
val name = "Max"
val age = 25
val message = "Mein Name is $name und ich bin $age Jahre alt."
println(message)
```

**Output:**
Mein Name ist Max und ich bin 25 Jahre alt.

In diesem Beispiel wird der String "message" durch die Verbindung von "name" und "age" dynamisch generiert. Um Variablen in einen String einzufügen, muss man lediglich ein "$" Zeichen vor den Variablennamen setzen.

## Tiefer Eintauchen

Es gibt noch weitere Möglichkeiten, Strings in Kotlin zu verbinden. Zum Beispiel können mehrere Strings mit dem "plus" Operator in einer Zeile verkettet werden:

```Kotlin
val firstName = "Anna"
val lastName = "Müller"
val fullName = firstName + " " + lastName
println(fullName)
```

**Output:**
Anna Müller

Wenn es darum geht, mehr als zwei Strings zu verbinden, gibt es auch die Funktion "plusAssign" (+=), die verwendet werden kann, um Strings an einen bestehenden String anzuhängen:

```Kotlin
var text = "Hallo"
text += " und Willkommen"
println(text)
```

**Output:**
Hallo und Willkommen

Es gibt auch die Funktion "StringBuilder", die für die Verkettung von großen Mengen an Strings effizienter ist. Dies ist besonders nützlich, wenn man beispielsweise eine Schleife verwendet, um mehrere Strings zusammenzufügen.

## Siehe auch

- [Kotlin Dokumentation: Type Conversions](https://kotlinlang.org/docs/tutorials/kotlin-for-py/loops.html) 
- [Kotlin Programmierhandbuch: String Concatenation](https://kotlinlang.org/docs/reference/basic-types.html#string-concatenation)