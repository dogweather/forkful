---
title:                "Debug-Ausgabe drucken"
html_title:           "Python: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum
Egal ob du ein Anfänger oder erfahrener Entwickler bist, das Drucken von Debug-Ausgaben ist ein wichtiger Schritt beim Entwickeln von Python-Programmen. Es hilft dabei, Fehler zu erkennen und den genauen Ablauf des Codes zu verstehen.

## Wie
```Python
# Ein einfaches Beispiel
name = "Max"
print("Der Name ist:", name)

# Ausgabe:
# Der Name ist: Max
```

```Python
# Debug-Ausgabe mit Variablen
a = 5
b = 10
c = a + b
print("Ergebnis:", c)

# Ausgabe:
# Ergebnis: 15
```

```Python
# Debug-Ausgabe in Schleifen
for i in range(3):
    print("Aktuelle Schleifenzahl:", i)

# Ausgabe:
# Aktuelle Schleifenzahl: 0
# Aktuelle Schleifenzahl: 1
# Aktuelle Schleifenzahl: 2
```

## Deep Dive
Das Drucken von Debug-Ausgaben kann mit dem `print()`-Befehl einfach und effektiv durchgeführt werden. Es kann auch mit anderen Funktionen wie `logging` oder Modulen wie `pdb` kombiniert werden, um detailliertere Ausgaben zu erhalten. Es ist auch wichtig, Debug-Ausgaben nur während der Entwicklung zu verwenden und sie dann zu entfernen, um die Leistung des finalen Programms nicht zu beeinträchtigen.

## Siehe auch
- [Python-Dokumentation zu `print()`](https://docs.python.org/de/3/library/functions.html#print)
- [Debuggen von Python-Code mit `pdb`](https://docs.python.org/de/3/library/pdb.html)
- [Effektives Debuggen in Python](https://realpython.com/python-debugging-pdb/)