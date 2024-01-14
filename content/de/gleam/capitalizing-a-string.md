---
title:                "Gleam: Großschreibung eines Strings"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man vielleicht einen String im Programmieren großschreiben möchte. Ein häufiger Grund ist, um Konsistenz und Lesbarkeit im Code zu gewährleisten, besonders bei der Darstellung von Benutzereingaben oder Ausgabe an den Benutzer.

## Wie geht's

Die Gleam-Programmiersprache bietet ein einfaches und effektives Mittel, um einen String in Großbuchstaben umzuwandeln. Mit der Funktion `String.capitalize` können wir einen beliebigen String übergeben und erhalten als Ergebnis den selben String, jedoch mit dem ersten Buchstaben großgeschrieben. Hier ist ein Beispiel:

```Gleam
let name = "max"
let capitalized_name = String.capitalize(name)
IO.print(capitalized_name)
```

Die Ausgabe wird sein:

```
Max
```

## Tiefergehend

Obwohl es einfach erscheinen mag, gibt es einige interessante Dinge zu beachten, wenn man einen String in Gleam großschreiben möchte. Zum Beispiel unterstützt die Funktion `String.capitalize` auch Unicode-Zeichen und Umlaute. Außerdem gibt es verschiedene Methoden, um nur einen Teil eines Strings in Großbuchstaben zu konvertieren oder um eine bestimmte Schreibweise beizubehalten. Durch die Verwendung von Gleams Pattern Matching-Konstrukten können wir auch komplexere Logik implementieren, um Strings in bestimmten Fällen zu großschreiben.

## Siehe auch

- [Gleam-Dokumentation: `String.capitalize`](https://gleam.run/docs/stdlib/string#capitalize)
- [Gleam-Dokumentation: Pattern Matching](https://gleam.run/docs/guide/pattern-matching)
- [Blog-Beitrag: Warum Konsistenz in der Programmierung wichtig ist](https://www.codementor.io/@oleksiychupryna/why-consistency-matters-in-programming-best-practices-ntn06709p)