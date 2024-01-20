---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/printing-debug-output.md"
---

{{< edit_this_page >}}

# Drucken von Debugging-Ausgaben mittels Python 

## Was & Warum

Drucken von Debugging-Ausgaben ist die Technik, um den Fluss und den Zustand in einem Programm zu überwachen. Entwickler nutzen sie, um Fehler effizient zu finden und zu beheben.

## Wie geht das?

In Python wird häufig die eingebaute `print()`-Funktion genutzt, um Debugging-Ausgaben zu erzeugen.

```python
def summe(zahlen):
    sum = 0
    for num in zahlen:
        sum += num
        print(f"Summe bis jetzt: {sum}")
    return sum

summe([5, 10, 15])
```

Ausgabe:

```
Summe bis jetzt: 5
Summe bis jetzt: 15
Summe bis jetzt: 30
```

Eine fortgeschrittenere Methode ist es, das `logging`-Modul zu verwenden. 

```python
import logging

logging.basicConfig(level=logging.DEBUG)

def summe(zahlen):
    sum = 0
    for num in zahlen:
        sum += num
        logging.debug(f"Summe bis jetzt: {sum}")
    return sum

summe([5, 10, 15])
```

Ausgabe:
 
```
DEBUG:root:Summe bis jetzt: 5
DEBUG:root:Summe bis jetzt: 15
DEBUG:root:Summe bis jetzt: 30
```

## Tiefere Infos

- Historischer Kontext: Die Druckausgabe wurde von den Anfängen der Programmierung an für Debugging-Zwecke genutzt. Sie ist weit verbreitet und Sprachübergreifend.
 
- Alternativen: Debugging-Werkzeuge oder IDEs bieten oft fortschrittlichere Möglichkeiten zur Code-Inspektion. In Python gibt es zum Beispiel `pdb`.

- Implementierungsdetails: Ein Logging-System wie das `logging`-Modul ist in der Regel flexibler als einfache `print()`-Aufrufe. Mit `logging` kann man zum Beispiel auch Informationen in Dateien schreiben, auf verschiedene Detailebenen einstellen, usw.

## Weitere Informationen

- Python `print()` in die [Python docs](https://docs.python.org/3/library/functions.html#print)
- Python `logging`-Modul in den [Python docs](https://docs.python.org/3/library/logging.html)
- Python Debugger (`pdb`) in den [Python docs](https://docs.python.org/3/library/pdb.html)