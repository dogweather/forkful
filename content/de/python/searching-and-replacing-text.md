---
title:                "Suchen und Ersetzen von Text"
html_title:           "Python: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal einen längeren Text geschrieben hast, kennst du das Problem: Plötzlich musst du mehrere Wörter oder Sätze ändern, die sich an verschiedenen Stellen im Text befinden. Anstatt alles manuell durchzugehen, kannst du die Suchen-und-Ersetzen-Funktion nutzen, um Zeit und Mühe zu sparen. In diesem Artikel zeige ich dir, wie du dieses nützliche Werkzeug in Python nutzen kannst.

## Wie es geht

Die Suchen-und-Ersetzen-Funktion in Python ist sehr einfach zu verwenden. Zunächst musst du den zu durchsuchenden Text in eine Variable speichern. Dann rufst du die `replace()`-Methode auf und gibst die zu ersetzenden Begriffe sowie ihre neuen Werte ein.

```Python
text = "Ich habe gestern eine Pizza gegessen."

neuer_text = text.replace("Pizza", "Burger")

print(neuer_text)
```

Die Ausgabe lautet: "Ich habe gestern eine Burger gegessen."

Es ist auch möglich, mehrere Begriffe gleichzeitig zu ersetzen und die Groß- und Kleinschreibung zu berücksichtigen:

```Python
text = "Heute ist ein wunderschöner Tag."

neuer_text = text.replace("heute", "morgen", 1).replace("Tag", "Abend")

print(neuer_text)
```

Die Ausgabe lautet: "Morgen ist ein wunderschöner Abend."

## Tiefergehende Informationen

Die `replace()`-Methode ersetzt nur den ersten gefundenen Begriff in einem Text. Möchtest du alle Begriffe ersetzen, musst du die `count`-Argument auf `0` setzen. Außerdem kannst du mithilfe von regulären Ausdrücken bestimmte Muster im Text suchen und ersetzen.

Sieh dir die offizielle Dokumentation von Python zu `str.replace()` an, um weitere Informationen und Beispiele zu erhalten.

## Siehe auch

- [Einführung in reguläre Ausdrücke in Python](https://python.de/tutorial/regex)
- [Offizielle Dokumentation zu str.replace()](https://docs.python.org/3/library/stdtypes.html#str.replace)