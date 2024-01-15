---
title:                "Unterstrings extrahieren"
html_title:           "Python: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals ein langes Wort oder einen Text hast, bei dem du nur an bestimmten Teilen interessiert bist, dann kann das Extrahieren von Teilen dieser Texte sehr nützlich sein. Dies kann besonders hilfreich sein, wenn es um das Manipulieren von Daten oder das Suchen nach bestimmten Mustern geht.

## Wie man es macht

Das Extrahieren von Teilstrings in Python ist relativ einfach. Wir werden die `slice`-Methode verwenden, um den Teilstring zu definieren, den wir aus unserem ursprünglichen String extrahieren möchten.

```Python
# Erstellen eines Beispielstrings
text = "Ich liebe Python!"

# Extrahieren des Wortes "liebe"
substring = text[3:8]

# Ausgabe des Ergebnisses
print(substring)
```

Die Ausgabe des obigen Codes wäre "liebe". Es ist wichtig zu beachten, dass beim Extrahieren von Teilstrings in Python die Indizierung bei 0 beginnt. In unserem Beispiel haben wir den Bereich von Index 3 bis Index 8 ausgewählt, wobei der erste Index inklusive und der letzte Index exklusive ist.

## Tiefes Eintauchen

Die `slice`-Methode in Python ermöglicht es uns, Teilstrings nicht nur von vorne, sondern auch von hinten zu extrahieren. Wir können dies tun, indem wir negative Indizes verwenden. Zum Beispiel würde der Ausdruck `text[:-4]` den Teilstring von Anfang bis vier Zeichen vor dem Ende zurückgeben.

Ein weiteres nützliches Feature ist die Möglichkeit, Schritte anzugeben, um Teilstrings auszuschneiden. Zum Beispiel kann der Ausdruck `text[::2]` jedes zweite Zeichen des Strings ausgeben. Das kann besonders hilfreich sein, wenn wir nach Mustern in einem Text suchen, die sich wiederholen.

## Siehe auch

- Dokumentation zu Python-Strings: https://docs.python.org/3/library/stdtypes.html#str
- Tutorials zu Python-Strings: https://www.w3schools.com/python/python_strings.asp