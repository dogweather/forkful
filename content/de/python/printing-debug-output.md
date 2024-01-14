---
title:    "Python: Debug-Ausgabe drucken"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren kann es oft zu unerwartetem Verhalten oder Fehlern kommen. Um diese Probleme zu lösen, kann es hilfreich sein, Debug-Ausgaben zu drucken, um den genauen Ablauf des Codes zu verfolgen. Dadurch können mögliche Fehlerquellen leichter identifiziert und behoben werden.

## Wie geht man vor

Um Debug-Ausgaben in Python zu drucken, gibt es verschiedene Methoden. Eine Möglichkeit ist die Verwendung der `print()` Funktion, die in Python zur Standardausgabe verwendet wird. Hier ein Beispiel:

```Python
# Debug-Ausgabe mit print() Funktion
x = 5
y = 10
print("Der Wert von x ist:", x)
print("Der Wert von y ist:", y)
```

Die Ausgabe dieses Codes würde folgendermaßen aussehen:

```
Der Wert von x ist: 5
Der Wert von y ist: 10
```

Eine andere Möglichkeit ist die Verwendung des `logging` Moduls, welches flexiblere Ausgabemöglichkeiten bietet. Hier ein Beispiel:

```Python
# Debug-Ausgabe mit logging Modul
import logging
x = 5
y = 10
logging.debug("Der Wert von x ist: %s", x)
logging.debug("Der Wert von y ist: %s", y)
```

Die Ausgabe dieses Codes wäre ähnlich wie bei der `print()` Funktion, jedoch könnte man hier z.B. auch den gewünschten Ausgabelevel festlegen.

## Tiefergehende Informationen

Beim Drucken von Debug-Ausgaben gibt es einige wichtige Aspekte zu beachten. Zum einen sollte man sicherstellen, dass die Ausgaben in einer Produktionsumgebung deaktiviert sind, um mögliche Leistungseinbußen zu vermeiden. Dazu kann man z.B. mit dem `__debug__` Flag arbeiten und Debug-Ausgaben nur bei aktiviertem Debug-Modus drucken. Außerdem können die Ausgaben auch formatiert werden, um sie anschaulicher zu gestalten.

Eine weiterführende Möglichkeit ist die Verwendung eines Debuggers, der es ermöglicht, Schritt für Schritt durch den Code zu gehen und Variablenwerte zu überprüfen.

## Siehe auch

- [Python Debugging with pdb](https://docs.python.org/3/library/pdb.html)
- [Python Logging HOWTO](https://docs.python.org/3/howto/logging.html)
- [Python Debugging Techniques](https://realpython.com/python-debugging/)