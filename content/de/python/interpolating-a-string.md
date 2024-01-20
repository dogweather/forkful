---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & warum?

String-Interpolation ist ein Prozess, bei dem wir Variablen in Textketten einfügen. Es ermöglicht Programmierern, dynamischen Text in ihre Programme einzufügen und fördert eine klarere und robustere Code-Schreibung.

## So geht's:

Wir haben zwei Hauptmöglichkeiten, Strings in Python zu interpolieren: mit der `.format()`-Methode und mit f-Strings. Hier sind zwei Beispiele:

```Python
# Mit der .format()-Methode
name = "Karl"
gruß = "Hallo, {}!".format(name)
print(gruß)  # Ausgabe: Hallo, Karl!

# Mit f-Strings
name = "Karl"
gruß = f"Hallo, {name}!"
print(gruß)  # Ausgabe: Hallo, Karl!
```

## Vertiefung

Die `.format()`-Methode wurde in Python 2.6 eingeführt und ermöglicht es den Programmierern, Platzhalter in ihrer String mit der in den Klammern angegebenen Variable zu ersetzen. Seit Python 3.6 gibt es eine elegantere Methode zur String-Interpolation: f-Strings. Die f-Strings können Ausdrücke innerhalb der Platzhalter auswerten, welcher mit dem `.format()`-Methode nicht möglich ist.

Es gibt auch ältere Methoden zur String-Interpolation in Python, wie die `%`-Methode, die aber als überholt gelten und daher in neuem Code vermieden werden sollten.

## Weitere Informationen

Für weitere Informationen zur String-Interpolation und den relevanten Python-Themen, überprüfen Sie die Python-Dokumentation über f-Strings unter diesem [Link](https://docs.python.org/3/reference/lexical_analysis.html#f-strings) und die `.format()`-Methode unter diesem [Link](https://docs.python.org/3/library/stdtypes.html#str.format).