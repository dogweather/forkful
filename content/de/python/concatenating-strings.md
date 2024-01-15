---
title:                "Zeichenfolgen verbinden"
html_title:           "Python: Zeichenfolgen verbinden"
simple_title:         "Zeichenfolgen verbinden"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Wer als Anfänger in der Programmierung versucht, Wörter oder Sätze miteinander zu verbinden, wird schnell auf die Methode der Stringkonkatenation stoßen. Doch warum ist es wichtig zu wissen, wie man Strings konkateniert?

Die simple Antwort ist, dass die Konkatenation von Strings eine grundlegende Fähigkeit ist, die in vielen Programmieraufgaben verwendet wird. Egal ob für die Erstellung von Nutzer-Ausgaben oder die Bearbeitung von Texten, die Kenntnis der Stringkonkatenation ist unerlässlich.

## Wie

Die Konkatenation von Strings in Python ist sehr einfach, da die Sprache spezielle Operatoren für diese Aufgabe bereitstellt. Die Syntax "string1 + string2" gibt als Ergebnis einen neuen String zurück, der aus der Verkettung von string1 und string2 besteht.

```Python
# Beispiel 1
# Verkettung von zwei einzelnen Wörtern
wort1 = "Hallo,"
wort2 = "Welt!"
ergebnis = wort1 + wort2
# Ausgabe: Hallo, Welt!

# Beispiel 2
# Verkettung von Zahlen und Strings
zahl = 42
ergebnis = "Die Antwort auf alles ist " + str(zahl)
# Ausgabe: Die Antwort auf alles ist 42
```

## Deep Dive

Obwohl die Stringkonkatenation in Python sehr einfach zu verstehen ist, gibt es einige Details, die es wert sind, näher betrachtet zu werden.

Die Operatoren "+" für die Verkettung und "*" für die Wiederholung von Strings, wie sie im vorherigen Abschnitt verwendet wurden, sind eigentlich spezielle Funktionen, die für Strings definiert sind. Dies bedeutet, dass sie nur für Strings verwendet werden können und nicht für andere Datentypen funktionieren.

Darüber hinaus ist es wichtig zu beachten, dass bei der Konkatenation von Strings stets ein neuer String erstellt wird. Wenn also in einer Schleife mehrere Strings konkateniert werden sollen, ist es effizienter, eine Liste zu verwenden und diese am Ende der Schleife mit der Funktion "join" zu einem einzigen String zusammenzufügen.

Beispielcode:
```Python
# Beispiel 3
# Konkatenation von Strings in einer Schleife
liste = ["Was", "dich", "nicht", "tötet,", "macht", "dich", "stärker."]
ergebnis = ""
for wort in liste:
    ergebnis += wort + " "
# Ausgabe: Was dich nicht tötet, macht dich stärker.

# Alternative Lösung mit der "join" Funktion
ergebnis = " ".join(liste)
```

Zuletzt sei noch erwähnt, dass Python auch den "+" Operator für die Verkettung von anderen Datentypen wie Listen oder Tupeln unterstützt. In diesem Fall wird jedoch nicht einfach ein neuer String erstellt, sondern es wird versucht, die beiden Datentypen miteinander zu verschmelzen. Dies kann zu unerwarteten Ergebnissen führen und sollte daher vermieden werden.

## Siehe auch

- [Offizielle Python-Dokumentation zur Stringkonkatenation](https://docs.python.org/de/3.9/tutorial/introduction.html#strings)
- [Weitere nützliche String-Methoden in Python](https://www.geeksforgeeks.org/python-string-methods-set-1-find-replace-split-swap-case/)