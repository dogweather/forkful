---
title:                "Fish Shell: Substrings extrahieren"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substring-Extraktion, auch bekannt als Teilzeichenfolgenextraktion, ist ein nützliches Werkzeug, um bestimmte Teile einer Zeichenfolge zu isolieren. Dies kann hilfreich sein, wenn man zum Beispiel nur die ersten 5 Zeichen einer ID-Nummer benötigt oder bestimmte Wörter aus einem längeren Text herausfiltern möchte.

##Wie funktioniert es

Um Substrings in der Fish Shell zu extrahieren, gibt es verschiedene Möglichkeiten. Eine davon ist die Verwendung des `string` Befehls. Hier ist ein Beispiel, um die ersten 3 Zeichen einer Zeichenfolge zu extrahieren und sie dann in Großbuchstaben auszugeben:

```Fish Shell
set text "Hallo Welt"
string sub $text 0 3 | string upper
```
Die Ausgabe wäre `HAL`.

## Tiefer tauchen

Um tiefer in die Substring-Extraktion einzutauchen, gibt es mehrere Parameter, die man verwenden kann. Zum Beispiel kann mit dem `length` Parameter die Anzahl der extrahierten Zeichen festgelegt werden. Mit dem `start` Parameter kann man angeben, ab welchem Index in der Zeichenfolge die Extraktion beginnen soll.

Zusätzlich gibt es auch die Möglichkeit, negative Zahlen für den `start` Parameter zu verwenden. Dies bedeutet, dass die Extraktion von rechts beginnt und nicht von links.

##Siehe auch

- Fish Shell-Dokumentation zu `string` Befehl
- Ein Tutorial zur Verwendung von Substrings in Fish Shell
- Eine Liste von weiteren nützlichen Befehlen in der Fish Shell