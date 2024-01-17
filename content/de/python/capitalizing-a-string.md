---
title:                "Eine Zeichenkette großschreiben"
html_title:           "Python: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
In Python können wir eine Zeichenkette (String) in Großbuchstaben (Capital letters) umwandeln. Wir machen das, damit die Ausgabe konsistenter und lesbarer wird.

## Wie geht's?
Wir können die Methode `.upper()` verwenden, um eine Zeichenkette in Großbuchstaben zu verwandeln. Sehen wir uns ein Beispiel an:

```Python
string = "hallo welt"
print(string.upper())
```

Die Ausgabe wäre: `HALLO WELT`.

## Tiefgründiges
Diese Methode existiert nicht nur in Python, sondern auch in anderen Programmiersprachen wie Java und C++. Eine Alternative zur `.upper()` Methode ist die Verwendung der `.capitalize()` Methode, die nur den ersten Buchstaben großschreibt. Einige Implementationen, wie z.B. in Python 2, haben auch die `.title()` Methode, die jeden Anfangsbuchstaben in einer Zeichenkette großschreibt.

## Siehe auch
- [Python String Methods](https://www.w3schools.com/python/python_strings_methods.asp)
- [Python 2 vs. Python 3: What's Different?](https://www.codecademy.com/articles/python-2-vs-python-3)
- [The History of Python Programming Language](https://www.edureka.co/blog/history-of-python/)