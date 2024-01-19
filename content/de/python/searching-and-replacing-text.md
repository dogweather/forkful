---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?

Suchen und Ersetzen von Text ist eine Operation, bei der bestimmte Zeichenketten (Worte oder Ausdrücke) in einem Text gefunden und durch andere ersetzt werden. Programmierer tun dies, um Textinhalte dynamisch zu aktualisieren, Fehler zu korrigieren oder Daten sauber zu formatieren.

## So geht's:

In Python verwenden wir die Methode `replace()` um Text zu suchen und zu ersetzen.

```Python
text = "Hallo, Welt!"
neuer_text = text.replace("Welt", "Python")
print(neuer_text)
```

Dies wird ausgeben:

```
Hallo, Python!
```

Du kannst auch angeben, wie oft der Text ersetzt werden soll.

```Python
text = "Ich mag Äpfel, Äpfel sind gesund."
neuer_text = text.replace("Äpfel", "Orangen", 1)
print(neuer_text)
```

Dies wird ausgeben:

```
Ich mag Orangen, Äpfel sind gesund.
```

## Tiefere Einblicke:

Die `replace()` Methode ist in Python seit seiner ersten Version verfügbar, sie ist also historisch gesehen ein sehr wichtiger Bestandteil von Pythons String-Verarbeitung.

Alternativen zur `replace()` Methode umfassen reguläre Ausdrücke (RegEx), die in Python durch das `re` Modul unterstützt werden. Mit ihnen kannst du komplexe Austauschaufgaben umsetzen.

Die `replace()` Methode arbeitet intern durch einen Durchlauf des Strings, vergleicht jedes Zeichen mit dem zu findenden String und ersetzt es, wenn es übereinstimmt. Es ist wichtig zu wissen, dass `replace()` ein neues String gibt und den ursprünglichen unverändert lässt.

## Siehe auch:

- [Python Official Docs - String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Python RegEx Tutorial](https://www.w3schools.com/python/python_regex.asp)
- [Stack Overflow Discussion on replace()](https://stackoverflow.com/questions/3559559/why-does-python-use-replace-and-replace)