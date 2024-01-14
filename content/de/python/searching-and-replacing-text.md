---
title:                "Python: Suchen und Ersetzen von Text"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Warum

In diesem Blog-Beitrag geht es darum, wie man mithilfe von Python Textsuche und -ersetzung durchführen kann. Das kann zum Beispiel in der Datenanalyse oder beim automatisierten Verarbeiten von Textdokumenten sehr hilfreich sein.

# So geht's

Um Textsuche und -ersetzung in Python durchzuführen, gibt es einige praktische Funktionen, die in der Standardbibliothek bereits verfügbar sind.

Wir beginnen mit der ```replace()``` Funktion, die in Strings verwendet wird, um bestimmte Zeichensequenzen durch andere zu ersetzen. Hier ein Beispiel:

```
my_string = "Hallo Welt!"
new_string = my_string.replace("Welt", "PyWelt")
print(new_string)
```

Die Ausgabe wird sein: "Hallo PyWelt!". 

Man kann auch mit regulären Ausdrücken Textsuche und -ersetzung durchführen. Dazu importiert man das "re" Modul und verwendet die ```sub()``` Funktion:

```
import re

my_string = "1234-5678-9012-3456"
new_string = re.sub(r"\D", "", my_string)
print(new_string)
```

Das Ergebnis wird sein: "1234567890123456". Hier haben wir mithilfe des regulären Ausdrucks "\D" alle Nicht-Ziffern durch Leerzeichen ersetzt.

# Tiefergehende Informationen

Um tiefer in die Thematik der Textsuche und -ersetzung einzutauchen, lohnt es sich, sich mit regulären Ausdrücken und der Python Standardbibliothek zu beschäftigen. Es gibt auch zahlreiche externe Bibliotheken, die noch mehr Funktionalitäten bieten, wie zum Beispiel "regex" oder "pyreplace".

In der offiziellen Python Dokumentation findet man ausführliche Informationen und Beispiele zur Nutzung der ```replace()``` und ```sub()``` Funktionen. Außerdem gibt es viele Online-Tutorials und Bücher, die sich ausführlicher mit diesem Thema beschäftigen.

# Siehe auch

- [Offizielle Python Dokumentation zu String-Funktionen](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Python Regular Expressions Tutorial von Real Python](https://realpython.com/regex-python/)
- [Externe Bibliothek "regex" für fortgeschrittene reguläre Ausdrücke](https://pypi.org/project/regex/)
- [Externe Bibliothek "pyreplace" für erweiterte Textsuche und -ersetzung](https://pypi.org/project/pyreplace/)