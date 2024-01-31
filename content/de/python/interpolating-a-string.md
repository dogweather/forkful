---
title:                "Interpolation eines Strings"
date:                  2024-01-28T21:23:41.871760-07:00
lastmod:               2024-01-29
model:                 gpt-4-0125-preview
simple_title:         "Interpolation eines Strings"

category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/interpolating-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
  - 2024-01-29, dogweather, nochmal überprüft
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation ist die Methode, Ausdrücke in String-Literale einzubetten. Programmierer verwenden sie, um dynamisch Werte in Strings einzufügen, was den Code lesbarer und sauberer macht als die traditionelle String-Konkatenation.

## Wie geht das:
In Python 3.6 und höher können Sie Strings mit f-Strings interpolieren. So geht's:

```Python
name = 'Alice'
alter = 30
gruß = f"Hallo, {name}. Du bist {alter} Jahre alt."

print(gruß)
```

Ausgabe:
```
Hallo, Alice. Du bist 30 Jahre alt.
```

Sie können auch Ausdrücke innerhalb der geschweiften Klammern verwenden:

```Python
a = 5
b = 10
info = f"Fünf plus zehn ist {a + b}, nicht {2 * (a + b)}."

print(info)
```

Ausgabe:
```
Fünf plus zehn ist 15, nicht 30.
```

## Tiefer Eintauchen
Vor Python 3.6 war `.format()` der Weg, um Strings zu interpolieren:

```Python
name = 'Bob'
alter = 25
gruß = "Hallo, {}. Du bist {} Jahre alt.".format(name, alter)

print(gruß)
```

Alte Schule Python (Versionen < 2.6) verwendete den `%`-Operator für die Interpolation, was weniger intuitiv ist und mit mehreren Variablen unübersichtlich werden kann:

```Python
name = 'Carol'
alter = 35
gruß = "Hallo, %s. Du bist %d Jahre alt." % (name, alter)

print(gruß)
```

Neben einer saubereren Syntax sind f-Strings schneller, weil sie zur Laufzeit ausgewertet und dann direkt in eine effiziente String-Format-Operation umgewandelt werden. Die `.format()`-Methode und der `%`-Operator beinhalten mehr Schritte und sind langsamer.

## Siehe auch
- [PEP 498 – Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/) für die offizielle Dokumentation zu f-Strings.
- [Python f-Strings](https://realpython.com/python-f-strings/) von Real Python für ein Tutorial zur Verwendung von f-Strings.
- [Die .format()-Methode](https://docs.python.org/3/library/stdtypes.html#str.format) in der Python-Dokumentation, um die ältere `.format()`-Methode der String-Formatierung zu verstehen.
