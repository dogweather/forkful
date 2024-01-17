---
title:                "Zeichenketten verknüpfen"
html_title:           "Python: Zeichenketten verknüpfen"
simple_title:         "Zeichenketten verknüpfen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/concatenating-strings.md"
---

{{< edit_this_page >}}

# Was & Warum?

Die Zusammenfügung von Strings, auch bekannt als Verkettung, ist ein Verfahren, bei dem zwei oder mehr Zeichenfolgen miteinander verbunden werden, um eine längere zu erstellen. Programmierer verwenden diese Technik häufig, um dynamisch Nachrichten, Dateipfade oder URLs zu erstellen.

# Wie geht's?

Die Konkatenation von Strings ist in Python sehr einfach und intuitiv. Sie können die Operator "+" verwenden, um Zeichenfolgen zusammenzuführen, oder die Methode "join" auf einer Liste von Strings anwenden.

Beispielcode:

```Python
# Verwendung von +
nachname = "Müller"
vorname = "Anna"
print(nachname + ', ' + vorname) # Ausgabe: Müller, Anna

# Verwendung von join
namensliste = ["Müller", "Anna"]
print(', '.join(namensliste)) # Ausgabe: Müller, Anna
```

# Tiefere Einblicke

Die Verkettung von Strings ist eine weit verbreitete Technik, die seit den Anfängen der Computerprogrammierung verwendet wird. Aufgrund der Flexibilität und Einfachheit wird sie auch in anderen Sprachen häufig verwendet.

Es gibt auch alternative Möglichkeiten, Strings zu concatenieren, wie z.B. die Verwendung von Formatierungsoptionen oder die Verwendung von String-Interpolation. Diese Techniken können in bestimmten Situationen nützlicher sein, aber die Verkettung bleibt eine einfach zu verstehende und effiziente Methode.

In Python werden Zeichenfolgen intern als Arrays von Unicode-Zeichen gespeichert. Beim Ausführen von Verkettungsvorgängen werden neue Zeichenfolgenobjekte erzeugt, was zu einem gewissen Overhead führen kann. Es ist daher empfehlenswert, bei vielen Verkettungen die "join"-Methode zu verwenden, um den Overhead zu minimieren.

# Sieh auch

Hier sind einige nützliche Ressourcen, um mehr über die Verkettung von Strings in Python zu erfahren:

- Python-Dokumentation "strings" (https://docs.python.org/3/library/string.html)
- Real Python Tutorial "Concatenating Strings in Python" (https://realpython.com/python-string-concatenation/)
- Codebeispiele auf GeeksforGeeks (https://www.geeksforgeeks.org/python-string-concatenation/)