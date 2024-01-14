---
title:    "Python: Die Länge eines Strings finden."
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings kann eine nützliche Fähigkeit in der Programmierung sein. Es ermöglicht es uns, die Anzahl der Zeichen in einem String zu ermitteln, was uns bei verschiedenen Aufgaben helfen kann, wie zum Beispiel bei der Validierung von Benutzereingaben oder der Manipulation von Texten.

## Wie

Die Länge eines Strings in Python kann ganz einfach mit der eingebauten Funktion `len()` gefunden werden. Schauen wir uns ein Beispiel an:

```Python
text = "Hallo Welt!"
print(len(text))
```

In diesem Beispiel erstellen wir eine Variable `text` mit dem Wert "Hallo Welt!" und verwenden dann die `len()` Funktion, um die Länge des Strings zu finden. Das Ergebnis wird dann mit `print` ausgegeben. Das Ergebnis dieses Codes wird 11 sein, da der String 11 Zeichen lang ist.

## Tiefer Einblick

In Python ist ein String im Grunde genommen eine Sequenz von Zeichen, die in Anführungszeichen gesetzt werden. Die `len()` Funktion zählt also einfach die Anzahl der Elemente in dieser Sequenz und gibt sie als Ergebnis zurück. Es ist wichtig zu beachten, dass Leerzeichen und Sonderzeichen auch in die Länge des Strings einbezogen werden.

## Siehe auch

- Offizielle Python-Dokumentation: https://docs.python.org/3/library/functions.html#len
- Stack Overflow-Beitrag zum Finden der Länge eines Strings: https://stackoverflow.com/a/6684386
- Codecademy-Lektion zum Arbeiten mit Strings in Python: https://www.codecademy.com/learn/learn-python-3/modules/learn-python3-strings/cheatsheet