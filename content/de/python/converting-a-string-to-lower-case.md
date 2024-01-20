---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Zeichenketten in Kleinbuchstaben umwandeln in Python: Eine einfache Anleitung

## Was & Warum?

Einen String (Kette von Zeichen) in Kleinbuchstaben umzuwandeln heißt einfach, jeden Großbuchstaben im String in seinen entsprechenden Kleinbuchstaben zu ändern. Programmierer machen das oft, um Fallunterschiede beim Vergleichen von Strings zu vermeiden.

## So wird's gemacht:

Python macht es uns mit der eingebauten Methode `.lower()` leicht. Schauen wir uns das an.

```Python
original_string = "Hallo Welt!"
lower_case_string = original_string.lower()

print(lower_case_string)
```

Wenn Sie dieses Skript ausführen, erhalten Sie:

```Python
'hallo welt!'
```

## Deep Dive

Das Konzept der String-Manipulation ist so alt wie die Programmierung selbst, entstand erstmals in den 1950er Jahren. Die Methode `.lower()` ist jedoch speziell auf die Bedürfnisse der Python-Community zugeschnitten.

Alternativ könnten wir eine manuelle Implementierung mit der Funktion `ord()` und `chr()` verwenden, aber das wäre ineffizienter und komplexer.

Interessanterweise wird `.lower()` in Python intern mit einer C-Funktion implementiert, was die Effizienz erhöht.

```Python
def to_lower(string):
    return ''.join(chr(ord(c) + 32) if 65 <= ord(c) <= 90 else c for c in string)

print(to_lower("Hallo Welt!"))
```

Ausgabe:

```Python
'hallo welt!'
```
In dieser Funktion wird für jeden Buchstaben im String `string` der ASCII-Wert ermittelt. Ist der ASCII-Wert im Bereich der Großbuchstaben (65-90), wird 32 addiert, um den entsprechenden Kleinbuchstaben zu erhalten, und dann zurück in einen Buchstaben umgewandelt. Sonst wird der ursprüngliche Charakter beibehalten.

## Siehe Auch

Weitere hilfreiche Quellen zum Vertiefen Ihrer Kenntnisse:

1. Die offizielle Python-Dokumentation zur String-Manipulation: [Python Docs](https://docs.python.org/3/library/stdtypes.html#str.lower)
2. Ein Tutorial zur Python-String-Manipulation auf Real Python: [Real Python Tutorial](https://realpython.com/python-strings/)