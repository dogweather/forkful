---
title:                "Löschen von Zeichen mit entsprechendem Muster"
html_title:           "Haskell: Löschen von Zeichen mit entsprechendem Muster"
simple_title:         "Löschen von Zeichen mit entsprechendem Muster"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine häufige Aufgabe in der Programmierung, bei der bestimmte Zeichen aus einem String entfernt werden, um das Ergebnis einer Berechnung oder eines Prozesses zu beeinflussen. Dies wird oft verwendet, um unerwünschte Zeichen zu entfernen oder um Daten in einem bestimmten Format zu bereinigen. Programmierer nutzen diese Technik, um String-Manipulationen effizient durchzuführen und die Ausgabe ihrer Programme zu verbessern.

# Wie geht's?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in Haskell auf verschiedene Arten erreicht werden. Eine Möglichkeit ist die Verwendung der Funktion "filter", die eine Liste von Elementen akzeptiert und nur diejenigen zurückgibt, die einer bestimmten Bedingung entsprechen. Zum Beispiel, um alle Vokale aus einem String zu löschen, könnten wir Folgendes schreiben:

```Haskell
filter (`notElem` "aeiou")
```

Das Muster "aeiou" entspricht allen Vokalen, so dass alle Zeichen, die nicht in diesem Muster enthalten sind, in der ausgegebenen Liste bleiben. Als Ergebnis erhalten wir einen String ohne Vokale. Hier ist ein vollständiges Beispiel:

```Haskell
löscheVokale :: String -> String
löscheVokale xs = filter (`notElem` "aeiou") xs

main = print (löscheVokale "Hallo Welt") -- Output: "Hll Wlt"
```

Eine andere Möglichkeit, Zeichen anhand eines Musters zu löschen, ist die Verwendung von regulären Ausdrücken mit der Bibliothek "Text.Regex". Diese Methode bietet mehr Flexibilität und kann komplexere Muster erkennen. Zum Beispiel, um alle Zahlen aus einem String zu löschen, könnten wir Folgendes schreiben:

```Haskell
import Text.Regex

löscheNummern :: String -> String
löscheNummern xs = subRegex (mkRegex "[0-9]+") xs ""

main = print (löscheNummern "a1b2c3") -- Output: "abc"
```

In diesem Beispiel verwenden wir die Funktion "subRegex", die Zeichen basierend auf einem regulären Ausdruck ersetzt. Im Ausdruck "[0-9]+" wird angegeben, dass alle Zahlen aus dem String entfernt werden sollen. Die leere Zeichenkette "" am Ende gibt an, dass diese durch nichts ersetzt werden sollen.

# Tief eintauchen

Das Löschen von Zeichen basierend auf einem Muster hat eine lange Geschichte in der Programmierung und ist eine grundlegende Technik in vielen Sprachen. In Haskell gibt es verschiedene Möglichkeiten, dies zu erreichen, unter anderem auch mithilfe von Bibliotheken wie "Data.Char", die spezielle Funktionen für die Behandlung von Charakteren bieten.

Alternativ zum Löschen von Zeichen können Programmierer auch andere Techniken verwenden, um Manipulationen an Zeichenketten durchzuführen, wie zum Beispiel das Ersetzen oder Hinzufügen von Zeichen. Dies wird oft für die Formatierung von Daten oder die Erzeugung anderer Zeichenketten verwendet.

Bei der Implementierung von Löschen von Zeichen basierend auf einem Muster ist es wichtig, die Leistung zu berücksichtigen, da es bei der Verarbeitung großer Datenmengen einen großen Unterschied machen kann, wie effizient diese Aufgabe ausgeführt wird.

# Siehe auch

- [Haskell Filter Funktion](https://www.haskell.org/hoogle/?hoogle=filter)
- [Haskell Regular Expressions Tutorial](https://wiki.haskell.org/Regular_expressions)
- [List of Useful Haskell Libraries](https://www.haskell.org/hoogle/)