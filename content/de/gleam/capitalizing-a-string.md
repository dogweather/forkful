---
title:    "Gleam: Großschreibung einer Zeichenkette"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Warum

Die Kapitalisierung von Strings kann ein nützliches Tool sein, um Texte in bestimmten Fällen besser lesbar zu machen oder um bestimmte formale Richtlinien einzuhalten. Zum Beispiel können Benutzernamen oder Überschriften durch die Kapitalisierung vereinheitlicht werden.

##Wie geht man vor?

Die Kapitalisierung in Gleam ist ganz einfach! Man kann den eingebauten `String.capitalize()` Befehl nutzen, um den ersten Buchstaben eines Strings zu einem Großbuchstaben zu machen. Hier ein Beispiel:

```Gleam
my_string = "hallo"
kapitalisiert = String.capitalize(my_string)
```

Die Ausgabe im Falle von `kapitalisiert` wird "Hallo" sein. 

Es ist auch möglich, alle Buchstaben in einem String zu kapitalisieren mit `String.uppercase()`. Hier ein Beispiel, wo beide Befehle genutzt werden:

```Gleam
mein_string = "GUTEN ABEND"
normalisiert = String.lowercase(my_string)
kapitalisiert = String.capitalize(normalisiert)
```

Die Ausgabe hier wird wieder "Guten Abend" sein. 

##Tiefergehende Information

Die Kapitalisierung von Strings in Gleam nutzt die [Unicode character properties](https://unicode.org/reports/tr21/). Das heißt, dass es auch in anderen Sprachen als Englisch funktioniert und Sonderzeichen sowie Akzente korrekt behandelt. Es ist jedoch wichtig zu beachten, dass bestimmte Zeichenkodierungen und Schriften möglicherweise für die korrekte Kapitalisierung konfiguriert werden müssen.

##Siehe auch

- [Gleam Dokumentation über Strings](https://gleam.run/documentation/stdlib/String.html)
- [Gleam Dokumentation über Unicode](https://gleam.run/documentation/stdlib/Unicode.html)
- [Unicode Character Database](https://www.unicode.org/reports/tr44/#General_Category_Values)