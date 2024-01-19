---
title:                "Einen String großschreiben"
html_title:           "Python: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Großschreibung eines Strings in Python bedeutet, dass wir den ersten Buchstaben des Strings in einen Großbuchstaben umwandeln. Programmierer verwenden diese Technik häufig, um Text korrekt zu formatieren und leserlicher zu machen.

## Wie geht es?
Um einen String in Python zu kapitalisieren, nutzen wir die eingebaute Methode `.capitalize()`. Hier ist ein Codebeispiel dazu:

```Python
s = "hallo welt"
s = s.capitalize()
print(s)
```

Die Ausgabe dieses Codeausschnitts wäre:

```Python
'Hallo welt'
```

## Tiefgang
Die Großschreibung von Text kann auf eine laaaaaange Geschichte in der Typografie und Drucktechnik zurückblicken. In Python, entstand die `.capitalize()` Methode schon in dessen ersten Versionen.

Alternativ zur Methode `.capitalize()`, können Sie auch die Methode `.title()` verwenden, die jeden ersten Buchstaben eines Wortes groß schreibt:

```Python
s = "hallo welt"
s = s.title()
print(s)
```

Die Ausgabe wäre:

```Python
'Hallo Welt'
```

Die Methode `.capitalize()` funktioniert so: Der Python-Interpreter wandelt den ersten Buchstaben des Strings in eine entsprechende Unicode Großbuchstabenänderung um.

## Siehe auch
Für weitere Informationen, Sie können diese Links besuchen:

1. Offizielle Python-Dokumentation über Strings: https://docs.python.org/3/library/stdtypes.html#string-methods
2. Wikipediaseite über String (Informatik): https://de.wikipedia.org/wiki/String_(Informatik)
3. Ein Python-Tutorial auf Deutsch: https://www.python-lernen.de/