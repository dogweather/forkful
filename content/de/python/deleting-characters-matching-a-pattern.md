---
title:                "Python: Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in verschiedenen Situationen nützlich sein. Zum Beispiel könnte es helfen, unerwünschte Zeichen aus einer Textdatei zu entfernen, oder beim Scraping von Daten aus dem Internet den Text zu bereinigen.

## Wie man es macht

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann mit Hilfe von regulären Ausdrücken in Python durchgeführt werden. Hier ist ein einfaches Beispiel, das alle Zahlen aus einem String entfernt:

```Python
import re
text = "Dies ist ein Beispiel123 Text"
clean_text = re.sub('[0-9]+', '', text)
print(clean_text)
```

Die Ausgabe wäre: "Dies ist ein Beispiel Text". In diesem Beispiel wird die Funktion `re.sub()` verwendet, um alle Zahlen aus dem String zu entfernen. Das erste Argument gibt das Muster an, nach dem gesucht werden soll (hier `[0-9]+` für jede beliebige Ziffer), das zweite Argument ist der String, in dem gesucht werden soll, und das dritte Argument ist der String, der anstelle des gefundenen Musters eingefügt werden soll.

Eine weitere nützliche Methode ist `re.findall()`, die alle Vorkommnisse eines Musters in einem String in einer Liste zurückgibt. Hier ist ein Beispiel, das alle Wörter mit mindestens vier Buchstaben aus einem String extrahiert:

```Python
import re
text = "Dies ist ein Beispiel123 Text with English words"
words = re.findall('[a-zA-Z]{4,}', text)
print(words)
```

Die Ausgabe wäre: `['Dies', 'ist', 'Beispiel', 'Text', 'with', 'English', 'words']`. In diesem Beispiel wird das Muster `[a-zA-Z]{4,}` verwendet, um nach Wörtern mit mindestens vier Buchstaben zu suchen.

## Tieferer Einblick

Um besser zu verstehen, wie reguläre Ausdrücke in Python funktionieren, ist es hilfreich, sich mit den verschiedenen Metazeichen und Ausdrücken vertraut zu machen. Dazu gehören zum Beispiel `+` für ein oder mehrere Vorkommnisse, `*` für null oder mehr Vorkommnisse und `[ ]` für eine Gruppe von Zeichen. Es gibt auch spezielle Sequenzen wie `\d` für eine beliebige Ziffer und `\w` für ein beliebiges alphanumerisches Zeichen.

Eine weitere wichtige Methode, die es sich anzusehen lohnt, ist `re.compile()`, mit der man einen regulären Ausdruck kompilieren und wiederverwenden kann. Dies kann insbesondere bei der Verarbeitung großer Mengen an Texten nützlich sein.

Für eine detaillierte und umfassende Erklärung zu regulären Ausdrücken in Python empfehlen wir die offizielle Dokumentation unter [https://docs.python.org/3/library/re.html](https://docs.python.org/3/library/re.html).

## Siehe auch

- [Eine Einführung in reguläre Ausdrücke in Python](https://realpython.com/regex-python/)
- [RegExr - Ein interaktiver Regulärer-Ausdruck Tester für Python](https://regexr.com/)
- [RegEx Cheat Sheet für Python](https://cheatography.com/mutanclan/cheat-sheets/python-regular-expression-regex/)