---
title:    "Python: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Programmierung sehr nützlich sein, um ungewünschte Daten zu entfernen oder Daten in einem bestimmten Format zu bearbeiten.

# How To

Um Zeichen in einem String zu löschen, die einem bestimmten Muster entsprechen, können Sie die `re` Bibliothek in Python verwenden. Zuerst importieren wir die Bibliothek:

```python
import re
```

Dann definieren wir unseren String und das Muster, das wir löschen möchten:

```python
txt = "Mein Geburtsdatum ist am 01/01/2000."
pattern = "[0-9]"
```

Das Muster `[0-9]` bedeutet, dass wir alle Ziffern von 0 bis 9 löschen möchten. Nun wenden wir die `re.sub()` Funktion an, um unsere Zeichenfolge zu ändern:

```python
new_txt = re.sub(pattern, "", txt)
print(new_txt)
```

Die Ausgabe wäre:

`Mein Geburtsdatum ist am //.`

Wie Sie sehen können, wurden alle Ziffern in unserem String gelöscht. Sie können auch komplexere Muster verwenden, um bestimmte Zeichen oder Wörter zu löschen. Hier ist ein Beispiel, in dem wir alle Sonderzeichen und Leerzeichen aus einem String entfernen:

```python
txt = "Hallo! Wie geht es dir?"
pattern = "[^A-Za-z0-9]"
new_txt = re.sub(pattern, "", txt)
print(new_txt)
```

Die Ausgabe wäre:

`HalloWiegehtesdir`

# Deep Dive

Das `re` Modul in Python bietet eine breite Palette an Funktionen und Möglichkeiten, um mit regulären Ausdrücken zu arbeiten. Mit dem `re.sub()` Befehl können Sie nicht nur Zeichen löschen, sondern auch ersetzen oder hinzufügen, je nachdem, welche Argumente Sie der Funktion übergeben. Es gibt auch verschiedene Optionen und Flags, die Sie verwenden können, um Ihre regulären Ausdrücke anzupassen.

Um mehr über die Verwendung von regulären Ausdrücken in Python zu erfahren, können Sie die offizielle Dokumentation der `re` Bibliothek lesen oder verschiedene Tutorials online finden.

# Siehe auch

- Offizielle Dokumentation der `re` Bibliothek: https://docs.python.org/de/3/library/re.html
- Tutorial über reguläre Ausdrücke in Python: https://realpython.com/regex-python/
- Einführung in reguläre Ausdrücke in Python: https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial