---
title:                "Python: Verwendung von regulären Ausdrücken"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein mächtiges Werkzeug in der Python-Programmierung. Sie erlauben es uns, komplexe Muster in Texten zu erkennen und zu manipulieren. Durch die Verwendung von regulären Ausdrücken können wir effizienter und präziser arbeiten und so unseren Code verbessern.

## Wie man sie verwendet

Reguläre Ausdrücke werden in Python mit dem Modul `re` implementiert. Um dieses Modul zu verwenden, müssen wir es zunächst importieren:

```Python
import re
```

Ein grundlegendes Beispiel für die Verwendung von regulären Ausdrücken ist die Suche nach einer bestimmten Zeichenkette in einem Text. Dazu verwenden wir die Funktion `search()` und geben den regulären Ausdruck sowie den Text als Parameter an:

```Python
text = "Dies ist ein Beispieltext."
ergebnis = re.search("ist", text)
```

Das Ergebnis dieser Suche ist ein sogenanntes Match-Objekt. Um zu überprüfen, ob die Suche erfolgreich war, können wir die Methode `group()` verwenden, die uns die gefundene Zeichenkette zurückgibt:

```Python
print(ergebnis.group()) # Ausgabe: "ist"
```

## Tiefergehende Informationen

Reguläre Ausdrücke bieten weit mehr Möglichkeiten als nur die Suche nach einer bestimmten Zeichenkette. Wir können beispielsweise auch bestimmte Zeichenklassen wie Ziffern oder Buchstaben suchen, Quantifizierer verwenden, um die Anzahl der zu findenden Zeichen anzugeben, oder Gruppierungen nutzen, um Teile eines Ausdrucks zu markieren.

Beispiel 1: Suche nach einer ISBN-Nummer

```Python
text = "Die ISBN-Nummer ist 978-3-446-25893-2."
isbn = re.search("\d{3}-\d-\d{6}-\d", text) # \d steht für eine Ziffer, \d{3} bedeutet also drei Ziffern
if isbn:
    print(isbn.group()) # Ausgabe: "978-3-446-25893-2"
```

Beispiel 2: Suche nach allen Zeichenkombinationen, die mit einem @-Symbol beginnen

```Python
text = "Meine Email-Adresse ist max@beispiel.com."
emails = re.findall("@\w+\.\w+", text) # \w steht für einen beliebigen Buchstaben oder eine Ziffer, + bedeutet mindestens einmal
if emails:
    print(emails) # Ausgabe: ["@beispiel.com"]
```

Für eine detaillierte Beschreibung aller verfügbaren Funktionen und Syntax-Elemente empfehle ich die offizielle Dokumentation des Moduls `re` sowie weitere Tutorials im Internet.

## Siehe auch

- [Offizielle Dokumentation des Moduls re](https://docs.python.org/3/library/re.html)
- [Weitere Informationen und Tutorials zu regulären Ausdrücken in Python](https://www.tutorialspoint.com/python/python_reg_expressions.htm)
- [Interaktiver regulärer Ausdruck-Tester](https://regex101.com/) (englisch)