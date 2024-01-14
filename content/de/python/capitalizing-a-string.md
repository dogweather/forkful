---
title:    "Python: Ein String großschreiben"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum 

Das Kapitalisieren von Strings ist eine häufige Aufgabe in der Python-Programmierung. Es kann verwendet werden, um Strings für eine bessere Lesbarkeit in der Ausgabe zu formatieren, in Datenbankabfragen oder für die Verwendung in Schlüssel- und Wertpaaren. Es ist eine einfache, aber nützliche Fähigkeit, die jeder Python-Programmierer beherrschen sollte.

# Wie geht man vor 

Es gibt mehrere Möglichkeiten, Strings in Python zu kapitalisieren. Die einfachste Methode ist die Verwendung der eingebauten Funktion `upper()`.

```Python
text = "dieser string wird kapitalisiert"

print(text.upper())
# Ausgabe: DIESER STRING WIRD KAPITALISIERT
```

Eine weitere Möglichkeit ist die Verwendung der Methode `capitalize()`, die nur den ersten Buchstaben des Strings groß macht.

```Python
text = "dieser string wird kapitalisiert"

print(text.capitalize)
# Ausgabe: Dieser string wird kapitalisiert
```

Um jeden ersten Buchstaben eines Wortes in einem String groß zu machen, kann die Funktion `title()` verwendet werden.

```Python
text = "dieser string wird kapitalisiert"

print(text.title())
# Ausgabe: Dieser String Wird Kapitalisiert
```

Wenn der String bereits teilweise großgeschrieben ist, kann die Methode `casefold()` verwendet werden, um alle Buchstaben in Kleinbuchstaben umzuwandeln und dann die Methode `capitalize()` oder `title()` anzuwenden.

```Python
text = "Das ist ein String mit teilweise großen Buchstaben"

text = text.casefold()
print(text.capitalize())
# Ausgabe: Das ist ein string mit teilweise großen buchstaben

print(text.title())
# Ausgabe: Das Ist Ein String Mit Teilweise Großen Buchstaben
```

# Tiefer Einblick 

Strings in Python werden als unveränderliche Sequenzen von Zeichen behandelt. Dies bedeutet, dass sie nach der Erstellung nicht mehr verändert werden können. Daher erzeugen alle oben genannten Methoden neue Strings und ändern nicht den ursprünglichen String.

Es ist auch wichtig zu beachten, dass die oben genannten Methoden je nach Spracheinstellung des Systems unterschiedlich funktionieren können. Zum Beispiel werden Umlaute in der deutschen Sprache nicht richtig in Großbuchstaben umgewandelt, wenn die Systemsprache auf Englisch eingestellt ist.

In Python gibt es auch die Möglichkeit, eigene Funktionen zu definieren, um Strings nach individuellen Kriterien zu kapitalisieren. Beispielsweise kann eine Funktion erstellt werden, die nur die ersten Buchstaben von Substrings zwischen Leerzeichen kapitalisiert.

# Siehe auch 

- [Python string methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Python string formatting](https://www.geeksforgeeks.org/python-formatting-output-using-string-formats/)
- [Python documentation](https://docs.python.org/3/library/stdtypes.html#string-methods)