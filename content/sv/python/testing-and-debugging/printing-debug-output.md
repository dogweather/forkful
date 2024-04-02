---
date: 2024-01-20 17:53:08.069507-07:00
description: "Att skriva ut debuginformation inneb\xE4r att visa vad som h\xE4nder\
  \ i din kod under k\xF6rningen. Programmerare g\xF6r detta f\xF6r att f\xF6rst\xE5\
  \ fl\xF6det, hitta buggar och\u2026"
lastmod: '2024-03-13T22:44:37.485685-06:00'
model: gpt-4-1106-preview
summary: "Att skriva ut debuginformation inneb\xE4r att visa vad som h\xE4nder i din\
  \ kod under k\xF6rningen. Programmerare g\xF6r detta f\xF6r att f\xF6rst\xE5 fl\xF6\
  det, hitta buggar och\u2026"
title: "Skriva ut fels\xF6kningsdata"
weight: 33
---

## Vad & Varför?
Att skriva ut debuginformation innebär att visa vad som händer i din kod under körningen. Programmerare gör detta för att förstå flödet, hitta buggar och optimera prestandan.

## How to:
Du kan använda `print()` för enklast möjliga debug:

```python
x = "Hej, världen!"
print(x)  # Skriver ut: Hej, världen!
```

För mer detaljerad info, använd f-string:

```python
value = 10
print(f'Värdet är {value}')  # Skriver ut: Värdet är 10
```

Ibland vill du skriva ut flera variabler. Separera med kommatecken:

```python
name = "Anders"
age = 42
print(name, age)  # Skriver ut: Anders 42
```

## Deep Dive:
`print()` är gammal som gatan men fortfarande användbar. Historiskt sett hade man mer primitiva sätt att debugga, som att skriva till filer eller terminalen utan någon form av formatering.

Alternativ? `logging` är en kraftfull kumpan. Det låter dig välja nivåer som ERROR, INFO och DEBUG och output kan lätt riktas om, till exempel till filer. Implementationen är enkel:

```python
import logging

logging.basicConfig(level=logging.DEBUG)
logging.debug('Detta är ett debug-meddelande.')
```

Men `print()` vinner när du snabbt vill se något utan krångel.

## See Also:
Kolla in Python-dokumentationen på:
- Print-funktionen: https://docs.python.org/3/library/functions.html#print
- Logging-modulen: https://docs.python.org/3/library/logging.html

Även denna artikel om `print()` vs `logging` är en pärla:
- Real Python's "Python Debugging With Print()": https://realpython.com/python-debugging-with-print/
