---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:10.265074-07:00
description: "String interpolatie is de methode van het invoegen van expressies binnen\
  \ string literals. Programmeurs gebruiken het om dynamisch waarden in strings in\
  \ te\u2026"
lastmod: '2024-03-13T22:44:50.362337-06:00'
model: gpt-4-0125-preview
summary: "String interpolatie is de methode van het invoegen van expressies binnen\
  \ string literals. Programmeurs gebruiken het om dynamisch waarden in strings in\
  \ te\u2026"
title: Een string interpoleren
weight: 8
---

## Wat & Waarom?
String interpolatie is de methode van het invoegen van expressies binnen string literals. Programmeurs gebruiken het om dynamisch waarden in strings in te voegen, wat de code leesbaarder en schoner maakt dan traditionele stringconcatenatie.

## Hoe:
In Python 3.6 en hoger, kun je strings interpoleren met f-strings. Zo werkt het:

```Python
name = 'Alice'
age = 30
greeting = f"Hallo, {name}. Je bent {age} jaar oud."

print(greeting)
```

Output:
```
Hallo, Alice. Je bent 30 jaar oud.
```

Je kunt ook expressies gebruiken binnen de accolades:

```Python
a = 5
b = 10
info = f"Vijf plus tien is {a + b}, niet {2 * (a + b)}."

print(info)
```

Output:
```
Vijf plus tien is 15, niet 30.
```

## Diepere duik
Voor Python 3.6 was de `.format()` methode de manier om strings te interpoleren:

```Python
name = 'Bob'
age = 25
greeting = "Hallo, {}. Je bent {} jaar oud.".format(name, age)

print(greeting)
```

Old school Python (versies < 2.6) gebruikte de `%` operator voor interpolatie, wat minder intuïtief is en rommelig kan worden met meerdere variabelen:

```Python
name = 'Carol'
age = 35
greeting = "Hallo, %s. Je bent %d jaar oud." % (name, age)

print(greeting)
```

Naast een schonere syntax zijn f-strings sneller omdat ze tijdens runtime worden geëvalueerd en dan direct worden omgezet in een efficiënte string format operatie. De `.format()` methode en `%` operator omvatten meer stappen en zijn trager.

## Zie ook
- [PEP 498 – Literal String Interpolation](https://www.python.org/dev/peps/pep-0498/) voor de officiële documentatie over f-strings.
- [Python f-strings](https://realpython.com/python-f-strings/) door Real Python voor een tutorial over het gebruik van f-strings.
- [De .format() Methode](https://docs.python.org/3/library/stdtypes.html#str.format) in de Python documentatie om de oudere `.format()` methode van stringformatting te begrijpen.
