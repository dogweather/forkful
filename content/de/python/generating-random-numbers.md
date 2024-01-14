---
title:    "Python: Erzeugung zufälliger Zahlen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist eine nützliche Fähigkeit für jeden Programmierer. Es ermöglicht die Erstellung von realistischen Simulationen, die Verschlüsselung von Daten und vieles mehr.

## Wie geht's

Die Verwendung von Zufallszahlen in Python ist einfach. Importiere einfach das `random` Modul und benutze die `random.randint()` Funktion, um eine Zufallszahl innerhalb eines bestimmten Bereichs zu erhalten. Zum Beispiel:

```Python
import random

# Generiere eine Zufallszahl zwischen 1 und 10
zufallszahl = random.randint(1, 10)

# Gib die Zahl aus
print(zufallszahl)

# Mögliche Ausgabe: 5
```

Es gibt auch andere Funktionen im `random` Modul, wie z.B. `random.random()`, um eine Zufallszahl zwischen 0 und 1 zu erhalten, oder `random.choice()` um eine zufällige Auswahl aus einer Liste zu treffen. Experimentiere mit ihnen, um mehr über die Verwendung von Zufallszahlen in Python zu erfahren.

## Tiefer Einblick

In der Informatik ist es schwierig, vollständig zufällige Zahlen zu generieren. Stattdessen werden Pseudozufallszahlen verwendet, die auf einem spezifischen Algorithmus basieren. Diese Zahlenfolgen sind nicht wirklich zufällig, aber sie erscheinen zufällig und erfüllen die meisten Anforderungen.

Es ist wichtig zu beachten, dass die gleiche Zufallszahlensequenz jedes Mal ausgegeben wird, wenn das Programm ausgeführt wird, es sei denn, es wird ein anderer Startwert festgelegt. Dies ist nützlich für die Fehlersuche, aber wenn echte Zufälligkeit erforderlich ist, sollten externe Quellen wie Rauschen oder Benutzereingaben verwendet werden.

## Siehe auch

- Offizielle Dokumentation für das `random` Modul: https://docs.python.org/3/library/random.html
- Tutorial zu Zufallszahlen in Python: https://www.python-kurs.eu/zufallszahlen.php
- Python-Kochbuch: Zufallszahlen: https://www.python-kurs.eu/kochbuch/zufallszahlen/index.php