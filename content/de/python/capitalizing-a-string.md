---
title:                "Eine Zeichenfolge großschreiben"
html_title:           "Python: Eine Zeichenfolge großschreiben"
simple_title:         "Eine Zeichenfolge großschreiben"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings kann hilfreich sein, wenn wir sicherstellen wollen, dass alle Wörter in einem Satz oder einer Phrase großgeschrieben sind, um eine einheitliche Darstellung zu gewährleisten oder bestimmte Formattierungsvorschriften zu erfüllen.

## Wie geht man vor?

Um einen String in Python zu kapitalisieren, können wir die integrierte Funktion "capitalize()" verwenden, die den ersten Buchstaben eines Strings großschreibt und alle anderen Buchstaben in kleinbuchstaben umwandelt. Schauen wir uns ein Beispiel an:

```Python
text = "hallo welt!"
print(text.capitalize())
```

Das obige Beispiel gibt uns die Ausgabe "Hallo welt!".

Eine weitere Möglichkeit ist die Verwendung der "title()" Funktion, die jeden einzelnen Wortanfang des Strings großschreibt. Schauen wir uns auch hier ein Beispiel an:

```Python
text = "python ist eine tolle sprache"
print(text.title())
```

Die Ausgabe hier lautet "Python Ist Eine Tolle Sprache".

## Tiefer eintauchen

In Python gibt es auch die Möglichkeit, eine benutzerdefinierte Funktion zum Kapitalisieren von Strings zu erstellen. Diese Funktion könnte verschiedene Methoden wie "split()" oder "join()" verwenden, um jeden einzelnen Wortanfang des Strings großzuschreiben. Wenn Sie mehr über die Implementierung solcher Funktionen erfahren möchten, können Sie sich die verschiedenen Methoden zur Zeichenkettenmanipulation in der Python-Dokumentation ansehen.

## Siehe auch

- Dokumentation zu Zeichenkettenmethoden in Python: https://docs.python.org/de/3/library/stdtypes.html#string-methods
- Weitere hilfreiche Python-Tutorials und Artikel: https://realpython.com/python-beginner-tips/