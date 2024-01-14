---
title:    "Python: Strings concatenieren"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum
Das Verketten von Zeichenketten ist ein wesentlicher Bestandteil der Programmierung in Python. Es ermöglicht das Zusammenfügen von Texten und Variablen, um dynamische Outputs zu erstellen. Dies ist besonders hilfreich bei der Erstellung von Strings für Benutzeroberflächen oder die Datenmanipulation.

## Wie man es macht
Die Verkettung von Zeichenketten in Python ist einfach und intuitiv. Dazu verwenden wir den `+` Operator, der als Concatenation Operator bekannt ist. Schauen wir uns ein Beispiel an:

```Python
# Definiere zwei Variablen mit Text
name = "Lena"
greeting = "Hallo"

# Verbinde die Variablen und einen zusätzlichen Text
message = greeting + ", " + name + "!"

# Gebe die Nachricht aus
print(message)
```

Die Ausgabe dieses Codes wäre `Hallo, Lena!`. Wie man sehen kann, können wir Strings und Variablen einfach mit dem `+` Operator verbinden, um eine längere Zeichenkette zu erstellen.

Eine weitere Möglichkeit, Zeichenketten zu verketten, ist die Verwendung der `format()` Funktion. Diese erlaubt es uns, Variablen innerhalb eines Textes einzufügen und so komplexere Nachrichten zu erstellen. Hier ist ein Beispiel:

```Python
# Definiere eine Variable für das Alter
age = 25

# Verwende die `format()` Funktion, um die Variable in einen Text einzusetzen
text = "Ich bin {} Jahre alt und liebe das Programmieren!"

# Gebe die Nachricht aus
print(text.format(age))
```

Die Ausgabe dieses Codes wäre `Ich bin 25 Jahre alt und liebe das Programmieren!`. Wie man sieht, können wir die `format()` Funktion verwenden, um Variablen innerhalb von Strings einzusetzen und so dynamische Nachrichten zu erstellen.

## Tiefergehende Einblicke
Beim Verketten von Zeichenketten gibt es noch einige zusätzliche Funktionen in Python, die hilfreich sein können. Eine davon ist die `join()` Funktion, mit der wir eine Liste von Strings zu einer Zeichenkette verbinden können. Hier ist ein Beispiel:

```Python
# Definiere eine Liste mit Namen
names = ["Anna", "Max", "Lisa"]

# Verwende die `join()` Funktion, um die Namen zu verketten
message = ", ".join(names)

# Gebe die Nachricht aus
print("Meine Freunde sind: " + message)
```

Die Ausgabe dieses Codes wäre `Meine Freunde sind: Anna, Max, Lisa`. Wie man sehen kann, können wir mit der `join()` Funktion eine Liste von Strings zu einer Zeichenkette verketten.

## Siehe auch
- [String Methods in Python](https://www.geeksforgeeks.org/python-string-methods-set-1-find-replace-split-count-etc/)
- [Python String Concatenation](https://www.w3schools.com/python/gloss_python_string_concat.asp)
- [An Introduction to Strings in Python](https://realpython.com/python-strings/)