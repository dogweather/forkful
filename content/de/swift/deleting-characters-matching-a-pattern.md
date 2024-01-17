---
title:                "Löschen von Zeichen entsprechend einem Muster"
html_title:           "Swift: Löschen von Zeichen entsprechend einem Muster"
simple_title:         "Löschen von Zeichen entsprechend einem Muster"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine gängige Aufgabe in der Programmierung. Oftmals wird diese Funktion verwendet, um unerwünschte Zeichen aus einer Zeichenkette zu entfernen oder um bestimmte Formatierungen in Texten zu ändern.

# Wie geht's?
Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in Swift mit der Methode `removeAll(matching:)` durchgeführt werden. Hier ist ein Beispiel einer Zeichenkette, aus der alle Leerzeichen entfernt werden sollen:

```Swift
let string = "H e l l o"
let newString = string.removeAll(matching: " ")
print(newString) // "Hello"
```

# Tiefgang
Das Löschen von Zeichen, das einem bestimmten Muster entspricht, ist keine neue Technik und wird schon seit längerer Zeit in der Programmierung verwendet. Es gibt auch alternative Methoden, um diese Aufgabe zu erledigen, wie zum Beispiel die Verwendung von Schleifen und bedingten Anweisungen. Die `removeAll(matching:)` Methode ist jedoch die einfachste und effektivste Möglichkeit, um Zeichen basierend auf einem bestimmten Muster zu löschen.

# Siehe auch
Weitere Informationen zu den `removeAll(matching:)` Methode und anderen String-Manipulationsfunktionen finden Sie in der offiziellen Swift-Dokumentation unter https://developer.apple.com/documentation/swift/string oder auf anderen Programmier-Websites wie Stack Overflow.