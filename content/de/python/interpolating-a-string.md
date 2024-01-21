---
title:                "Zeichenketten interpolieren"
date:                  2024-01-20T17:51:19.050843-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten interpolieren"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

String-Interpolation ist das Einbetten von Variablen in Strings. Programmierer nutzen das, weil's den Code lesbarer und das Zusammenbauen von Nachrichten flexibler macht.

## Anleitung:

String-Interpolation in Python kann schnell und schmerzlos sein. Hier ist, wie's geht:

```Python
name = "Welt"
begrüßung = f"Hallo, {name}!"
print(begrüßung)
```

Ausgabe:
```
Hallo, Welt!
```

Und noch ein Beispiel mit einer Zahl:

```Python
alter = 25
nachricht = f"Ich bin {alter} Jahre alt."
print(nachricht)
```

Ausgabe:
```
Ich bin 25 Jahre alt.
```

## Tiefer Tauchen

Früher war das Leben ohne die f-Strings (seit Python 3.6) ein bisschen komplizierter. Man musste `"{}".format(variable)` oder gar `%`-Operatoren verwenden, um eine String-Interpolation zu bewerkstelligen. Heutzutage verwenden viele Sprachen ähnliche Features, wie `${variable}` in JavaScript.

In Python kannst du auch Ausdrücke in die Klammern packen, das macht's noch mächtiger:

```Python
preis = 19.99
anzahl = 3
rechnung = f"Gesamtpreis: {anzahl * preis:.2f}€"
print(rechnung)
```

Das `.2f` ist dabei ein Format-Specifier, der die Float-Zahl auf zwei Dezimalstellen rundet.

## Siehe Auch:

- Die Python-Dokumentation zur String-Interpolation: https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting
- PEP 498, beschreibt die f-Strings: https://www.python.org/dev/peps/pep-0498/
- Ein Tutorial zu `.format()`: https://realpython.com/python-string-formatting/#2-new-style-string-formatting-strformat