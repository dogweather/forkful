---
title:                "Python: Strings verbinden."
simple_title:         "Strings verbinden."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Strings ist eine grundlegende Operation in der Python-Programmierung. Es ermöglicht es uns, mehrere Zeichenketten zu einem zusammenzufügen und somit längere und aussagekräftigere Ausgaben zu erzeugen.

## Wie man

In Python können wir Strings auf verschiedene Arten verketten. Die einfachste Möglichkeit ist die Verwendung des `+` Operators, wie in diesem Beispiel:

```Python
# Verketten von zwei Strings
anfang = "Hallo "
ende = "Welt!"
begrüßung = anfang + ende
print(begrüßung)
```

Die Ausgabe dieses Codes ist:

```
Hallo Welt!
```

Wir können auch den `+=` Operator verwenden, um einen bestehenden String zu erweitern:

```Python
# Erweitern eines bestehenden Strings
name = "Max"
name += " Mustermann"
print(name)
```

Die Ausgabe ist:

```
Max Mustermann
```

Eine weitere Möglichkeit ist die Verwendung von `str.join()`, um mehrere Strings mit einem Trennzeichen zu verketten:

```Python
# Verketten von Strings mit einem Trennzeichen
namen_liste = ["Max", "Mustermann", "Musterfrau"]
trennzeichen = " "
name = trennzeichen.join(namen_liste)
print(name)
```

Die Ausgabe ist:

```
Max Mustermann Musterfrau
```

## Tiefer Einblick

Strings in Python sind unveränderlich, was bedeutet, dass sie nicht direkt geändert werden können. Wenn wir also einen String verketten, erzeugen wir tatsächlich einen neuen String, anstatt den bestehenden zu ändern. Dies hat Auswirkungen auf die Leistung, insbesondere wenn wir eine große Anzahl von Strings verketten.

Um dieses Performance-Problem zu lösen, kann die Verwendung von `str.format()` anstelle von `+` oder `+=` eine bessere Wahl sein. Mit dieser Methode können wir Platzhalter in einem String definieren und dann Werte in diese Platzhalter einfügen, anstatt Strings direkt zu verketten.

```Python
# Verwendung von str.format()
zahl = 5
verkettung = "Ich habe {} Äpfel und {} Orangen".format(zahl, 3)
print(verkettung)
```

Die Ausgabe ist:

```
Ich habe 5 Äpfel und 3 Orangen
```

## Siehe auch

- Offizielle Python-Dokumentation zu Strings: https://docs.python.org/de/3/tutorial/introduction.html#strings
- Weitere Möglichkeiten, Strings in Python zu verarbeiten: https://www.datacamp.com/community/tutorials/python-string-processing