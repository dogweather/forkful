---
title:    "Python: String in Kleinbuchstaben umwandeln"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum
Das Konvertieren einer Zeichenkette in Kleinbuchstaben kann in vielen Situationen nützlich sein, zum Beispiel beim Vergleichen von Strings oder beim Formatieren von Benutzereingaben.

## So geht's
```Python
# Beispiel einer Zeichenkette
string = "HALLO, WELT!"

# Konvertieren zu Kleinbuchstaben und Ausgabe des Ergebnisses
print(string.lower())
# Ausgabe: hallo, welt!
```

Sie können auch die Methode `.casefold()` verwenden, um eine Zeichenkette in Kleinbuchstaben zu konvertieren. Diese Methode berücksichtigt auch Unicode-Zeichen und erzeugt somit ein einheitliches Ergebnis, unabhängig von der Sprache.

```Python
# Beispiel einer Zeichenkette mit Unicode-Zeichen
string = "İstanbul"

# Konvertieren zu Kleinbuchstaben und Ausgabe des Ergebnisses
print(string.casefold())
# Ausgabe: i̇stanbul
```

## Tiefere Einblicke
Die Verwendung von `.lower()` oder `.casefold()` kann bei der Vergleich von Zeichenketten entscheidend sein. Beispielsweise würde beim Vergleich von "Schule" und "schule" ohne die Konvertierung zu Kleinbuchstaben ein Ergebnis von `False` zurückgegeben werden.

Auch bei der Verarbeitung von Benutzereingaben ist es wichtig, die Eingaben auf Kleinbuchstaben zu konvertieren, da Benutzer möglicherweise nicht immer die richtige Groß- oder Kleinschreibung verwenden.

## Siehe auch
- [Offizielle Python Dokumentation zur `.lower()` Methode](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Offizielle Python Dokumentation zur `.casefold()` Methode](https://docs.python.org/3/library/stdtypes.html#str.casefold)
- [Webseite mit einer detaillierteren Erklärung zur Konvertierung von Zeichenketten](https://realpython.com/python-string-formatting/#string-case-conversion)