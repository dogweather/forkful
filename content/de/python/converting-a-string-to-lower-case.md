---
title:    "Python: Umwandlung eines Strings in Kleinbuchstaben"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Eine häufige Aufgabe in der Programmierung ist das Konvertieren eines Strings in Kleinbuchstaben. Dies kann aus verschiedenen Gründen nützlich sein, zum Beispiel um die Benutzerinteraktion zu vereinfachen oder um die String-Vergleiche zu erleichtern.

## Wie man

Das Konvertieren eines Strings in Kleinbuchstaben ist in Python sehr einfach. Hier ist ein Beispielcode:

```python
# Eingabe
string = "HALLO WELT"

# Konvertierung in Kleinbuchstaben
lower_string = string.lower()

# Ausgabe
print(lower_string)
```

Die Ausgabe dieses Codes ist "hallo welt".

In diesem Beispiel verwenden wir die `lower()` Methode, die auf einem String-Objekt aufgerufen wird und eine neue Version des Strings in Kleinbuchstaben zurückgibt. Es ist wichtig zu beachten, dass diese Methode den ursprünglichen String nicht verändert, sondern einen neuen zurückgibt. Wenn wir also den ursprünglichen String behalten wollen, müssen wir ihn in einer neuen Variablen speichern.

## Tiefergehende Informationen

In Python gibt es verschiedene Funktionen und Methoden, um einen String in Kleinbuchstaben umzuwandeln. Die `lower()` Methode ist die einfachste und am häufigsten verwendete. Sie funktioniert auch mit Sonderzeichen und Umlauten, was für Deutsch besonders wichtig ist.

Wenn Sie mehr Kontrolle über den Konvertierungsprozess wünschen, können Sie auch die `str.lower()` Funktion verwenden. Diese Funktion funktioniert ähnlich wie die `lower()` Methode, gibt jedoch immer einen String zurück, unabhängig davon, ob es sich um ein String-Objekt oder eine String-Variable handelt.

Eine weitere nützliche Funktion ist `casefold()`, die ähnlich wie `lower()` funktioniert, aber zusätzliche Transformationen anwendet, um auch spezielle Unicode-Zeichen wie Griechisch und Kyrillisch in Kleinbuchstaben umzuwandeln.

In einigen Fällen kann es auch sinnvoll sein, eine Konvertierung in Kleinbuchstaben auf Zeichen für Zeichen-Basis durchzuführen. Dies kann mit der `lower()` Funktion und einer Schleife um jeden Buchstaben des Strings erreicht werden.

Es ist wichtig zu beachten, dass es in Python auch Funktionen gibt, um einen String in Großbuchstaben umzuwandeln, wie zum Beispiel `upper()` und `casefold()`, die sich auf dieselbe Weise verhalten wie die entsprechenden Funktionen zum Konvertieren in Kleinbuchstaben.

## Siehe auch

- [Offizielle Python-Dokumentation zu `str.lower()`](https://docs.python.org/de/3/library/stdtypes.html#str.lower)
- [Python String Formatting - A Tutorial](https://realpython.com/python-string-formatting/)
- [10 Tips for Using Python String Methods](https://www.datacamp.com/community/tutorials/10-tricks-for-using-python-string-methods)