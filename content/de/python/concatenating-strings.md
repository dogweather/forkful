---
title:                "Python: Zeichenketten verketten"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Strings sind eine grundlegende Datentyp in Python, die verwendet werden, um Text und Zeichenfolgen darzustellen. Oftmals müssen wir jedoch mehrere Strings miteinander verbinden, um bestimmte Aufgaben zu erledigen. Die Konkatenation von Strings ist eine wichtige Fähigkeit, die jeder Python-Programmierer beherrschen sollte.

## Wie man Strings in Python konkateniert

Die einfachste Methode, Strings in Python zu konkatenieren, ist die Verwendung des Plusoperators (+). Schauen wir uns ein Beispiel an:

```Python
name = "Maria"
greeting = "Hallo, " + name + "!"
print(greeting)
```
Der obige Code würde die Ausgabe "Hallo, Maria!" erzeugen, da die einzelnen Strings durch den Plusoperator miteinander verbunden werden.

Eine andere Möglichkeit, Strings zu konkatenieren, ist die Verwendung der .format()-Methode. Hier ist ein Beispiel:

```Python
name = "Maria"
greeting = "Hallo, {}!".format(name)
print(greeting)
```
Dies würde die gleiche Ausgabe wie zuvor erzeugen, aber diesmal wird der Platzhalter {} durch den Wert von name ersetzt.

Schließlich können auch f-Strings verwendet werden, um Strings zu konkatenieren. F-Strings sind mit Python 3.6 eingeführt worden und ermöglichen es uns, Variablen direkt in einem String zu verwenden, indem sie mit dem Präfix "f" gekennzeichnet werden. Zum Beispiel:

```Python
name = "Maria"
greeting = f"Hallo, {name}!"
print(greeting)
```
Auch hier würde die Ausgabe "Hallo, Maria!" lauten.

## Tiefere Einblicke

Eines der wichtigsten Dinge, die man bei der Konkatenation von Strings beachten sollte, ist, dass es nur möglich ist, Strings mit anderen Strings zu verbinden. Versuche, Strings mit anderen Datentypen wie Integer oder Float zu verbinden, werden zu einem Fehler führen.

Außerdem ist es möglich, mehr als zwei Strings gleichzeitig zu konkatenieren, indem man einfach mehrere Einzelheiten mit dem Plusoperator verbindet. Zum Beispiel:

```Python
name = "Maria"
greeting = "Hallo, " + name + "!" + " Wie geht es dir?"
print(greeting)
```
Die Ausgabe wäre "Hallo, Maria! Wie geht es dir?".

Es gibt auch bestimmte Methoden, die in Python verfügbar sind, um die Konkatenation von Strings effektiver zu gestalten, wie z.B. .join() oder .split(). Es empfiehlt sich, diese Methoden bei Bedarf zu recherchieren.

## Siehe auch

- [Python String Concatenation](https://www.programiz.com/python-programming/string-concatenation)
- [Python String Methods](https://www.w3schools.com/python/python_strings_methods.asp)
- [PEP 498: Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/)