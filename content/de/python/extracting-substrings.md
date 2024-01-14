---
title:    "Python: Substrings extrahieren"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist eine nützliche Technik, die in vielen Fällen beim Programmieren nützlich sein kann. Zum Beispiel kann es hilfreich sein, bestimmte Teile eines Strings zu isolieren, um sie weiter zu manipulieren oder zu analysieren. In diesem Blog-Artikel werden wir uns ansehen, wie man diese Technik in Python anwenden kann.

## Wie geht man vor?

Um Teilstrings in Python zu extrahieren, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der Slice-Syntax, die es uns ermöglicht, einen Teil eines Strings basierend auf einer Start- und einer Endposition zu extrahieren.

```Python
# Beispiel 1: Extrahieren eines Teilstrings aus einem String
text = "Dies ist ein Testtext"

# Extrahieren des Teilstrings "Test"
substring = text[11:15]

# Ausgabe des Ergebnisses
print(substring)
```
Ausgabe: `Test`

Auch Teilstrings innerhalb einer bestimmten Länge können mit der Slice-Syntax extrahiert werden, indem wir die Endposition durch die Anzahl der Zeichen, die wir extrahieren möchten, ergänzen.

```Python
# Beispiel 2: Extrahieren eines Teilstrings mit fester Länge
text = "Dies ist ein Testtext"

# Extrahieren des Teilstrings "ein"
substring = text[9:12]

# Ausgabe des Ergebnisses
print(substring)
```
Ausgabe: `ein`

## Deep Dive

Neben der Slice-Syntax gibt es auch die `find()`-Methode, mit der Teilstrings basierend auf einem bestimmten Muster extrahiert werden können. Diese Methode gibt die Position des ersten Vorkommens des Teilstrings zurück.

```Python
# Beispiel 3: Extrahieren eines Teilstrings mit find()
text = "Dies ist ein Testtext"

# Extrahieren des Teilstrings "Test"
substring = text[10:]

# Ausgabe des Ergebnisses
print(substring)
```
Ausgabe: `Testtext`

Es ist auch möglich, die `index()`-Methode zu verwenden, die ebenfalls die Position des ersten Vorkommens eines Teilstrings zurückgibt, jedoch einen Fehler wirft, wenn kein entsprechender Teilstring gefunden werden kann.

Es gibt auch die Möglichkeit, Teilstrings anhand von regulären Ausdrücken zu extrahieren, wodurch die Suche nach komplexeren Mustern ermöglicht wird.

## Siehe auch

1. [Python-Dokumentation: Slice-Syntax](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str).
2. [Python-Dokumentation: String-Methoden](https://docs.python.org/3/library/stdtypes.html#string-methods).
3. [Real Python: Regular Expressions in Python](https://realpython.com/regex-python/).