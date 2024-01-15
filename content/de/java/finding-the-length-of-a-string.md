---
title:                "Die Länge eines Strings finden."
html_title:           "Java: Die Länge eines Strings finden."
simple_title:         "Die Länge eines Strings finden."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn du Programmieren lernst, wirst du früher oder später auf die Notwendigkeit stoßen, die Länge von Zeichenketten (auch bekannt als Strings) in deinem Code zu finden. Dies ist besonders hilfreich, wenn du Textverarbeitungs- oder Datenverarbeitungsanwendungen schreibst.

## Wie geht man vor

Um die Länge einer Zeichenkette in Java zu finden, gibt es eine integrierte Methode namens `length()`, die auf einem String-Objekt aufgerufen werden kann. Diese Methode gibt die Anzahl der Zeichen in dem String zurück.

Im folgenden Beispiel werden wir einen String mit dem Wert "Hallo Welt" erstellen und die `length()`-Methode verwenden, um seine Länge zu finden:

```Java
String str = "Hallo Welt";
System.out.println(str.length());
```

Die Ausgabe dieses Codes wäre "11", da es 11 Zeichen in der Zeichenkette gibt (einschließlich des Leerzeichen).

## Tiefer Einblick

Die `length()`-Methode zählt die Anzahl der Unicode-Zeichen in einem String, was bedeutet, dass sie auch multibyte-Zeichen wie Emojis berücksichtigt. Wenn du also einen String hast, der Emojis enthält, wird die Länge, die von `length()` zurückgegeben wird, die Anzahl der Zeichen und nicht die Anzahl der Bytes sein.

Außerdem gibt es auch eine `length`-Attribut, das auf Arrays in Java angewendet werden kann und die Anzahl der Elemente im Array zurückgibt. Es ist wichtig zu beachten, dass `length()` eine Methode ist, während `length` ein Attribut ist.

## Siehe auch

- [Java String-Klasse](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Java Arrays](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/arrays.html)
- [Unicode](https://home.unicode.org/)