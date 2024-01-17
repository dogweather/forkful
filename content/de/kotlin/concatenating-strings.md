---
title:                "Zeichenketten verketten"
html_title:           "Kotlin: Zeichenketten verketten"
simple_title:         "Zeichenketten verketten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wenn du häufig mit Strings in deinem Kotlin Programm arbeitest, hast du sicher schon von der Funktion "concatenating strings" gehört. Dabei handelt es sich um das Zusammenführen von mehreren Strings zu einem einzigen. Dies kann nützlich sein, um längere Strings zu erzeugen oder um Variablen und Text zu kombinieren.

## Wie geht's?
Die Verkettung von Strings in Kotlin ist äußerst einfach. Du kannst entweder den "+" Operator verwenden oder die "plus()" Methode aufrufen. Zum Beispiel:

```Kotlin
val text1 = "Hallo "
val text2 = "Welt!"

// Mit dem "+" Operator:
val combined = text1 + text2 // Ergebnis: "Hallo Welt!"

// Mit der "plus()" Methode:
val combined = text1.plus(text2) // Ergebnis: "Hallo Welt!"
```

## Tief Tauchen
Die Verkettung von Strings wurde schon in früheren Programmiersprachen wie C++ und Java verwendet. In Kotlin gibt es jedoch noch eine dritte Möglichkeit, die sogenannte "String Interpolation". Dabei werden Variablen direkt in einen String eingefügt, ohne den Umweg über den "+" Operator oder die "plus()" Methode. Zum Beispiel:

```Kotlin
val name = "Sandra"
val age = 25
val sentence = "Mein Name ist $name und ich bin $age Jahre alt." // Ergebnis: "Mein Name ist Sandra und ich bin 25 Jahre alt."
```

Eine Alternative zur Verkettung von Strings ist der Einsatz von StringBuffer oder StringBuilder Objekten. Diese erlauben es, Strings effizient zusammenzuführen, da sie intern jeweils nur eine große String-Variable erstellen und verändern. Dadurch werden unnötige Kopien verhindert und die Performanz verbessert.

## Siehe auch
Weitere Informationen und Beispiele findest du auf der offiziellen Kotlin Webseite unter [https://kotlinlang.org/docs/reference/basic-types.html#strings] und [https://www.baeldung.com/kotlin/strings].