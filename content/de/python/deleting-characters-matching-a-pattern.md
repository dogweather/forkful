---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Löschen von Zeichen, die einem Muster entsprechen in Python

## Was und Warum?

Das Löschen von Zeichen, die einem Muster entsprechen, ist ein Prozess, bei dem Zeichen aus einer Zeichenkette entfernt werden, die einem bestimmten Suchmuster entsprechen. Programmierer tun dies, um Daten zu bereinigen und zu präzisieren.

## So geht's

In Python verwenden wir dafür die `sub()` Funktion des `re` Moduls. Hier sehen Sie, wie es funktioniert:

```Python
import re

str = "Hallo, Welt!"
pattern = "[,!]"

new_str = re.sub(pattern, "", str)
print(new_str)
```

Die Ausgabe wäre: "Hallo Welt"

## Deep Dive

Die Python `re` Bibliothek wurde basierend auf die ursprünglichen Unix-Programme für reguläre Ausdrücke entwickelt und ist seit der Python-Version 1.5 verfügbar. Es bietet eine reiche und flexible Syntax für Musterabgleich.

Alternativ können Sie die `translate()` und `maketrans()` Funktionen verwenden, um Zeichen aus einer Zeichenkette zu löschen. Hier ist ein Beispiel:

```Python
str = "Hallo, Welt!"
removal_chars = ",!"

new_str = str.translate(str.maketrans("", "", removal_chars))
print(new_str)
```

Die `maketrans()` Funktion erstellt eine Übersetzungstabelle, und die `translate()` Funktion verwendet diese Tabelle, um die zu entfernenden Zeichen zu ersetzen.

## Siehe auch

Hier sind einige Ressourcen, die Ihnen helfen könnten, tiefer in die Materie einzutauchen:

- [Python `re` Dokumentation](https://docs.python.org/3/library/re.html)
- [Erklärung der Python `translate()` Funktion](https://www.w3schools.com/python/ref_string_translate.asp)
- [Python `maketrans()` Funktion](https://www.w3schools.com/python/ref_string_maketrans.asp)