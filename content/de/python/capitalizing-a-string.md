---
title:                "Python: String in Großbuchstaben umwandeln"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Großschreibung von Strings ist eine grundlegende Funktion in der Python-Programmierung. Sie ermöglicht es, die erste Buchstabe eines Strings in einen Großbuchstaben umzuwandeln. Dies kann wichtig sein, um die Lesbarkeit oder das Erscheinungsbild von Texten in Programmen zu verbessern.

## Wie man es macht

Um einen String in Python zu kapitalisieren, gibt es verschiedene Möglichkeiten. Die einfachste Methode ist die Verwendung der Funktion `capitalize()`. Hier ein Beispiel:

```Python
text = "python ist eine tolle Programmiersprache"
print(text.capitalize())
```

Die Ausgabe für dieses Beispiel wäre:

```Python
Python ist eine tolle Programmiersprache
```

Eine andere Möglichkeit ist die Verwendung der Funktion `title()`, die die ersten Buchstaben jedes Wortes eines Strings in Großbuchstaben umwandelt. Hier ein Beispiel:

```Python
text = "python ist eine tolle Programmiersprache"
print(text.title())
```

Die Ausgabe für dieses Beispiel wäre:

```Python
Python Ist Eine Tolle Programmiersprache
```

Es ist auch möglich, die Großschreibung in einer eigenen Funktion zu implementieren. Hier ein Beispiel dafür:

```Python
def capitalize_string(text):
    first_letter = text[0]
    rest_of_string = text[1:]
    capitalized_string = first_letter.upper() + rest_of_string
    return capitalized_string

text = "python ist eine tolle Programmiersprache"
print(capitalize_string(text))
```

Die Ausgabe für dieses Beispiel wäre:

```Python
Python ist eine tolle Programmiersprache
```

## Tiefentauchen

Es gibt einige Dinge zu beachten, wenn es darum geht, Strings in Python zu kapitalisieren. Zum Beispiel können Sonderzeichen wie Akzente oder Umlaute problematisch sein, da sie je nach Funktion möglicherweise nicht richtig in Großbuchstaben umgewandelt werden. Außerdem sollte beachtet werden, dass Strings in Python unveränderlich sind, was bedeutet, dass die Funktionen `capitalize()` und `title()` nicht den ursprünglichen String verändern, sondern einen neuen String mit den entsprechenden Änderungen zurückgeben.

## Siehe auch

- [Offizielle Dokumentation von Python zu Strings](https://docs.python.org/de/3/library/stdtypes.html#string-methods)
- [Stack Overflow Beitrag über die Großschreibung von deutschen Umlauten](https://stackoverflow.com/questions/56402233/capitalizing-umlauts-in-python)
- [Python-Tutorial zur Arbeit mit Strings](https://www.w3schools.com/python/python_strings.asp)