---
title:                "Einen String großschreiben"
html_title:           "Swift: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum? Und Wofür?

Kapitalisieren eines Strings bedeutet, den ersten Buchstaben eines Satzes oder Wortes groß zu schreiben. Programmierer machen dies oft, um Benutzereingaben zu formatieren oder Text für die Ausgabe angemessen darzustellen.

# Wie man:
In Swift können wir den ersten Buchstaben eines Strings mit der Funktion `capitalized` groß schreiben. Hier ist ein einfacher Code und seine Ausgabe:

```Swift
let title = "swift programmierung"
let capitalizedTitle = title.capitalized
print(capitalizedTitle)
```
Ausgabe:
```Swift
Swift Programmierung
```
So einfach ist das, Strings in Swift zu kapitalisieren!

# Mehr Einzelheiten

Historisch gesehen wurde das Kapitalisieren von Strings in der Programmierung verwendet, um das einheitliche Format von Text zu gewährleisten. Kapitalisierte Wörter werden oft verwendet, um Überschriften, Titel und den Beginn neuer Sätze zu kennzeichnen.

Es gibt auch alternative Methoden zur Kapitalisierung von Strings in Swift, wie das Durchlaufen des Strings und das Kapitalisieren jedes ersten Zeichens, obwohl dies weniger effizient ist. Die `capitalized` Methode ist jedoch die einfachste und am besten geeignete Methode für die meisten Situationen.

Unter der Haube verwendet `capitalized` die Unicode-Informationen jedes Zeichens, um zu bestimmen, ob eine Kapitalisierung erforderlich ist. Dies gewährleistet die Genauigkeit und Effizienz der Funktion.

# Weitere Informationen

Hier sind einige weitere Quellen, die Sie möglicherweise hilfreich finden:

- [Apple's Swift String Dokumentation](https://developer.apple.com/documentation/swift/string/)
- [Verwendung von `capitalized` in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-words-in-a-string)
- [Swift String-Handling Guide](https://developer.apple.com/documentation/swift/string/)