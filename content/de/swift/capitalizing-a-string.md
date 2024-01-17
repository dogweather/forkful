---
title:                "Eine Zeichenkette großschreiben"
html_title:           "Swift: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Capitalizing einer Zeichenkette handelt es sich um die Umwandlung des ersten Buchstabens jedes Wortes in einem Satz in einen Großbuchstaben. Programmierer nutzen diese Funktion, um den Text übersichtlicher zu gestalten und die Lesbarkeit zu erhöhen.

## So geht's:

```Swift
let string = "ich gehe gerne spazieren."
let capitalizedString = string.capitalized

print(capitalizedString) // "Ich Gehe Gerne Spazieren."
```

## Tiefpunkt:

Das Capitalizing von Strings wird oft als "Title Case" bezeichnet und wurde früher in der Typographie verwendet, um Überschriften hervorzuheben. Neben der Nutzung des `capitalized`-Befehls gibt es auch alternative Methoden, um Strings zu kapitalisieren, wie zum Beispiel die Verwendung von `uppercased` oder die manuelle Manipulation über Schleifen.

## Siehe auch:

[Die Dokumentation des `capitalized`-Kommandos](https://developer.apple.com/documentation/swift/string/2998327-capitalized)