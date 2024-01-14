---
title:                "Python: : Lesen der Befehlszeilenargumente"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Befehlszeilenargumenten ist eine nützliche Fähigkeit in der Python-Programmierung. Es ermöglicht es uns, Benutzerinteraktion zu ermöglichen und unsere Programme an bestimmte Situationen anzupassen.

# Wie geht das?

Wir können Befehlszeilenargumente in Python mithilfe der `sys` Bibliothek auslesen. Hier ist ein Beispielcode, der die Argumente ausgibt:

```python
import sys

argumente = sys.argv
print(argumente)
```

Wenn wir nun dieses Skript mit dem Befehl `python script.py argument1 argument2` ausführen, wird unser Output `['script.py', 'argument1', 'argument2']` sein. Wie wir sehen können, werden die Argumente als Liste gespeichert und der Name des Skripts ist immer das erste Element in der Liste.

# Tiefer Einblick

Wir können auch bestimmte Argumente gezielt auslesen, indem wir auf ihre Indexposition in der Argumentenliste zugreifen. Zum Beispiel, wenn wir nur den ersten Argumentwert ausgeben wollen, können wir `argumente[1]` verwenden. Wir können auch überprüfen, ob ein bestimmtes Argument vorhanden ist, indem wir es mit dem in Python üblichen `in` Operator überprüfen.

Ein weiterer wichtiger Aspekt ist die Verwendung von Argumentoptionen. Diese ermöglichen es uns, bestimmte Funktionen oder Einstellungen für unser Programm anzugeben. In Python können wir Argumentoptionen mithilfe der `argparse` Bibliothek auslesen. Hier ist ein Beispielcode:

```python
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("-n", "--name", help="Gibt den Namen des Benutzers aus.", type=str)
parser.add_argument("-a", "--alter", help="Gibt das Alter des Benutzers aus.", type=int)

args = parser.parse_args()

print("Name:", args.name)
print("Alter:", args.alter)
```

Wenn wir nun `python script.py -n Max -a 24` ausführen, wird unser Output `Name: Max Alter: 24` sein. Wir können auch eine Hilfeseite für unsere Argumentoptionen erstellen, indem wir `parser.print_help()` verwenden.

# Siehe auch

- [Python-Dokumentation: sys](https://docs.python.org/3/library/sys.html)
- [Python-Dokumentation: argparse](https://docs.python.org/3/library/argparse.html)
- [Ein Tutorial für das Lesen von Befehlszeilenargumenten in Python](https://realpython.com/command-line-interfaces-python-argparse/)