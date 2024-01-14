---
title:                "Python: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

**Warum:** Eines der grundlegenden Konzepte beim Programmieren ist die Manipulation von Zeichenketten. In diesem Blog-Beitrag werden wir darüber sprechen, wie man mithilfe von Python bestimmte Zeichen, die einem bestimmten Muster entsprechen, löschen kann.

**Wie man es macht:** Es gibt verschiedene Möglichkeiten, Zeichenketten in Python zu bearbeiten, aber wir werden uns hier auf die Verwendung der `re` Bibliothek konzentrieren, die für "regular expressions" (reguläre Ausdrücke) steht. Mit regulären Ausdrücken können wir Muster definieren und in einer Zeichenkette nach Übereinstimmungen suchen. Schauen wir uns mal ein Beispiel an:

```
import re

text = "Hallo! Wie geht es dir? Ich heiße Lisa."
pattern = "[aeiouyAEIOUY]" # Definiert das Muster für Vokale
result = re.sub(pattern, "", text)
print(result) # Output: Hll! W ght s dr? ch hß Ls.
```

Hier haben wir das Muster `[aeiouyAEIOUY]` definiert, welches alle Vokale (Groß- und Kleinbuchstaben) in einer Zeichenkette erkennt. Dann haben wir die `sub()` Funktion von `re` verwendet, um alle Übereinstimmungen mit einem leeren String zu ersetzen, wodurch wir die Vokale aus dem Text entfernen. Dieses einfache Beispiel zeigt, wie nützlich reguläre Ausdrücke sein können, um Zeichenketten zu bearbeiten.

**Tiefere Einblicke:** Jetzt, da wir ein grundlegendes Verständnis für die Verwendung von regulären Ausdrücken haben, können wir uns tiefer damit beschäftigen, wie man Zeichenketten manipuliert, indem man bestimmte Zeichen basierend auf einem Muster löscht. Hier sind einige weitere Bespiele dafür, wie man `sub()` verwenden kann:

```
text = "Brause ist blau"
# Löscht alle Leerzeichen
result = re.sub("\s", "", text)
print(result) # Output: Brauseistblau

text = "1, 2, 3, 4, 5"
# Löscht alle Zahlen
result = re.sub("\d", "", text)
print(result) # Output: ", , , , "

text = "The quick brown fox jumps over the lazy dog"
# Löscht alle Konsonanten
result = re.sub("[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]", "", text)
print(result) # Output: e i o o u o e e a y o

text = "Passwort: $1xT2yZ3a"
# Löscht alle Sonderzeichen
result = re.sub("[!@#$%^&*()_+=`~{}|\[\]:'\"<>?,./\-]", "", text)
print(result) # Output: Passwort1xT2yZ3a
```

Man kann sehen, dass mit der Verwendung von regulären Ausdrücken sehr vielfältige Operationen auf Zeichenketten angewendet werden können. Das Erlernen und Verwenden von regulären Ausdrücken kann also sehr mächtig sein, wenn es darum geht, Zeichenketten zu bearbeiten.

**Siehe auch:** Wenn du mehr über reguläre Ausdrücke und deren Anwendung in Python erfahren möchtest, findest du hier einige hilfreiche Links:

- Offizielle Python Dokumentation zur `re` Bibliothek: https://docs.python.org/3/library/re.html
- Ein Tutorial zur Verwendung von regulären Ausdrücken in Python: https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial
- Reguläre Ausdrücke Cheat Sheet für Python: https://www.debuggex.com/cheatsheet/regex/python