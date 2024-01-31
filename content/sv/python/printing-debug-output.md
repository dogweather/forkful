---
title:                "Skriva ut felsökningsdata"
date:                  2024-01-20T17:53:08.069507-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/printing-debug-output.md"
---

{{< edit_this_page >}}

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
