---
title:                "Die Länge einer Zeichenkette finden"
html_title:           "Gleam: Die Länge einer Zeichenkette finden"
simple_title:         "Die Länge einer Zeichenkette finden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Die Länge eines Strings zu bestimmen, kann für viele Programmieraufgaben von großer Bedeutung sein. Zum Beispiel könnte es erforderlich sein, die Eingaben eines Benutzers auf eine bestimmte Anzahl von Zeichen zu begrenzen oder die Länge eines Passworts zu überprüfen. In diesem Artikel werden wir uns ansehen, wie man mit Gleam einfach die Länge eines Strings herausfinden kann.

# Wie geht's

Um die Länge eines Strings in Gleam zu bestimmen, können wir die built-in Funktion `String.length` nutzen. Diese Funktion nimmt als Argument einen String entgegen und gibt die Anzahl der Zeichen in diesem String als Integer zurück. Schauen wir uns ein Beispiel an:

```Gleam
mein_string = "Hallo Welt!"
länge = String.length(mein_string)
```

Hier haben wir zuerst einen String mit dem Inhalt "Hallo Welt!" in der Variable `mein_string` gespeichert. Dann nutzen wir die `String.length` Funktion, um die Länge des Strings zu bestimmen und das Ergebnis in der Variable `länge` zu speichern. Wenn wir nun den Wert von `länge` ausgeben, sehen wir, dass er 11 ist, da der String 11 Zeichen hat.

# Deep Dive

Wenn wir uns den Code der `String.length` Funktion genauer anschauen, sehen wir, dass sie tatsächlich nur ein Wrapper um die eingebaute Funktion `erlang:length` ist. Dies bedeutet, dass Gleam Strings effizient und schnell verarbeitet, da Erlang eine eingebaute Funktion zum Zählen von Elementen in Listen hat.

Ein wichtiger Punkt hier ist, dass die `String.length` Funktion die Länge des Strings in Charakteren und nicht in Bytes bestimmt. Dies ist wichtig, da einige Sonderzeichen in Unicode mehr als ein Byte belegen können und diese dann als ein Zeichen betrachtet werden. Gleam behandelt Unicode-Zeichen daher anders als die meisten anderen Sprachen und garantiert, dass die Länge eines Strings in Gleam immer der tatsächlichen Anzahl von Zeichen entspricht.

# Siehe auch

- [Die Gleam Standard Library](https://gleam.run/libraries/)
- [Die Gleam String Dokumentation](https://gleam.run/docs/strings/)
- [Der Gleam String Quellcode](https://github.com/gleam-lang/gleam_stdlib/blob/main/core/string.gleam)