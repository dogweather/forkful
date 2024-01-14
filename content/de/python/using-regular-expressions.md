---
title:                "Python: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein leistungsfähiges Werkzeug für die Verarbeitung von Text in Python. Sie ermöglichen es dir, komplexe Muster in Strings zu finden und zu manipulieren. Mit regulären Ausdrücken kannst du beispielsweise Text in einer bestimmten Struktur suchen, ersetzen oder extrahieren. Dies macht sie zu einem unverzichtbaren Werkzeug für die Verarbeitung von Text in einer Vielzahl von Anwendungen.

## Wie man reguläre Ausdrücke verwendet

Die Verwendung von regulären Ausdrücken in Python ist einfach. Zunächst musst du das Modul `re` importieren, um auf die regulären Ausdrücke zugreifen zu können. Dann kannst du verschiedene Funktionen wie `search()`, `match()` oder `findall()` verwenden, um reguläre Ausdrücke auf Strings anzuwenden.

Hier ist ein Beispiel für die Verwendung von regulären Ausdrücken, um eine E-Mail-Adresse aus einem String zu extrahieren:

```Python
import re

text = "Meine E-Mail-Adresse ist john.doe@example.com"
email = re.search(r"[a-z0-9]+@[a-z]+\.[a-z]+", text)
print(email.group())
```

In diesem Beispiel verwenden wir die `search()` Funktion, um nach dem Muster `[a-z0-9]+@[a-z]+\.[a-z]+` in unserem `text` String zu suchen. Das Ergebnis ist die vollständige E-Mail-Adresse `john.doe@example.com`.

## Tiefere Einblicke

Reguläre Ausdrücke haben eine ganze Reihe von Sonderzeichen und Quantifizierern, die verwendet werden können, um bestimmte Muster in Strings zu erkennen und zu manipulieren. Zum Beispiel kann der `*` Quantifizierer verwendet werden, um zu überprüfen, ob ein bestimmtes Zeichen beliebig oft in einem String vorkommt.

Hier ist ein Beispiel, das diesen Quantifizierer verwendet, um nach einer beliebigen Anzahl von Leerzeichen in einem String zu suchen und diese zu entfernen:

```Python
import re

text = "Dieser Text     enthält zu viele Leerzeichen."
clean_text = re.sub(r" +", " ", text)
print(clean_text)
```

In diesem Beispiel verwenden wir die `sub()` Funktion, um mit dem regulären Ausdruck ` +` alle aufeinanderfolgenden Leerzeichen im `text` String zu finden und durch ein einzelnes Leerzeichen zu ersetzen. Das Ergebnis ist der String `Dieser Text enthält zu viele Leerzeichen.`

## Siehe auch

- [Dokumentation zu regulären Ausdrücken in Python](https://docs.python.org/3/library/re.html)
- [RegExr - Online RegEx Tester](https://regexr.com/)
- [Python Regular Expressions - Eine umfassende Einführung](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)