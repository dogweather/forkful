---
title:                "Gleam: In einen String in Kleinbuchstaben umwandeln"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Warum
Wenn du jemals mit Strings in Gleam gearbeitet hast, hast du vielleicht bemerkt, dass sie manchmal in verschiedenen Fällen geschrieben werden können - wie zum Beispiel Groß- und Kleinschreibung. Das Konvertieren eines Strings in Kleinbuchstaben kann helfen, einheitliche Daten zu erhalten und Vergleiche oder Suchen zu erleichtern.

##Wie man Strings in Gleam in Kleinbuchstaben umwandelt
```Gleam
string.to_lower("Hallo Welt") // Ausgabe: "hallo welt"
string.to_lower("Gleam ist toll") // Ausgabe: "gleam ist toll"
```

##Tiefere Einblicke
Das Konvertieren eines Strings in Kleinbuchstaben mag einfach erscheinen, aber es gibt einige Dinge zu beachten. Zum Beispiel kann es je nach verwendeter Sprache oder Umgebung Unterschiede in der Art und Weise geben, wie bestimmte Buchstaben in Kleinbuchstaben umgewandelt werden. Ein weiterer wichtiger Punkt ist die Verwendung von Sonderzeichen oder Sprachen, die mehr als nur die 26 Buchstaben des Alphabets enthalten. Eine sorgfältige Überprüfung und Verwendung der richtigen Funktionen in Gleam kann helfen, diese Faktoren zu berücksichtigen und sicherzustellen, dass die Konvertierung korrekt durchgeführt wird.

##Siehe auch
- [String-Operationen in Gleam](https://gleam.run/documentation/standard-library/string.html)
- [Gleam Dokumentation](https://gleam.run/documentation/)