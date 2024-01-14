---
title:                "Python: Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Es gibt verschiedene Gründe, warum man in der Programmierung eine Zeichenfolge in Kleinschreibung umwandeln möchte. Dies kann dazu dienen, die Eingabe des Benutzers einheitlich zu gestalten oder bestimmte Zeichenketten einfacher zu vergleichen.

## Wie geht man vor
Um eine Zeichenfolge in Kleinbuchstaben umzuwandeln, gibt es in Python eine eingebaute Funktion namens `lower()`. Diese Funktion akzeptiert eine Zeichenfolge als Eingabe und gibt diese in Kleinschreibung zurück. Hier ist ein Beispielcode:

```python
text = "HALLO"
print(text.lower())
```

Dieses Stück Code gibt die Ausgabe `hallo` zurück. Es ist auch möglich, die `lower()` Funktion auf benutzerdefinierten Eingaben anzuwenden, zum Beispiel:

```python
text = input("Bitte geben Sie einen Text ein: ")
print(text.lower())
```
Ein Beispiel für die Ausgabe wäre, wenn der Benutzer "GUTEN Morgen" eingibt, würde die Ausgabe "guten morgen" sein. Dies kann besonders nützlich sein, wenn Sie eine Benutzereingabe auf bestimmte Schlüsselwörter oder Wörter überprüfen müssen.

## Tiefergehende Information
Wenn wir uns tiefergehend mit der Umwandlung von Zeichenfolgen in Kleinbuchstaben befassen, gibt es einige Dinge zu beachten. Zum Beispiel, die `lower()` Funktion gibt immer eine neue Zeichenfolge zurück, ohne die ursprüngliche zu ändern. Es ist auch wichtig zu wissen, dass sich die Ausgabe der `lower()` Funktion je nach Sprache und Zeichensatz unterscheiden kann.

Eine andere Möglichkeit, eine Zeichenfolge in Kleinbuchstaben umzuwandeln, ist die Verwendung von `casefold()`, welche sich besonders für den Umgang mit Sonderzeichen eignet. Es gibt auch die Option, die Groß- und Kleinschreibung zu ignorieren, indem man die `lower()` Funktion mit `casefold()` kombiniert.

Insgesamt ist die Umwandlung von Zeichenfolgen in Kleinbuchstaben ein einfacher und nützlicher Prozess in der Programmierung.

## Siehe auch
- [Python Dokumentation über die `lower()` Funktion](https://docs.python.org/de/3/library/stdtypes.html#str.lower)
- [Python Dokumentation über die `casefold()` Funktion](https://docs.python.org/de/3/library/stdtypes.html#str.casefold)