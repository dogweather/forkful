---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:30.353687-07:00
description: "Een REPL, of Read-Eval-Print Loop, is een programmeeromgeving die individuele\
  \ gebruikersinvoer neemt, deze uitvoert, en het resultaat aan de gebruiker\u2026"
lastmod: '2024-03-13T22:44:50.378366-06:00'
model: gpt-4-0125-preview
summary: Een REPL, of Read-Eval-Print Loop, is een programmeeromgeving die individuele
  gebruikersinvoer neemt, deze uitvoert, en het resultaat aan de gebruiker teruggeeft.
title: Het gebruik van een interactieve shell (REPL)
weight: 34
---

## Hoe:
Duik direct in Python's REPL door `python` in je commandoregel te typen. Eenmaal daar, test eenvoudige operaties of meerdere regels code:

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
... 
0
1
2
```

Experimenteer met functies en onmiddellijke feedback:

```Python
>>> def greet(name):
...     return "Hallo, " + name + "!"
... 
>>> greet("Alice")
'Hallo, Alice!'
```

Speel met bibliotheken en verken hun functies in realtime:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

Verlaat met een snel `exit()` of `Ctrl+D` (soms `Ctrl+Z` op Windows).

## Diepgaande Duik
Het concept van een REPL is niet uniek voor Python; het is zo oud als Lisp. Veel talen bieden deze onmiddellijke, interactieve omgeving voor een praktische benadering van code. Alternatieven voor de native Python shell zijn onder meer IPython en Jupyter Notebook, die verbeterde interactiviteit, meer functies en betere integratie met andere tools bieden. Python's standaard REPL is eenvoudig, maar het bevat de volledige kracht van Python, het afhandelen van complexe objecten en multi-threaded programma's, hoewel het functies mist zoals automatisch aanvullen en syntaxiskleuring die in geavanceerdere tools aanwezig zijn.

## Zie Ook
- [Python's officiële documentatie over de interpreter](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: Een geavanceerde Python shell](https://ipython.org/)
- [Jupyter Project](https://jupyter.org/)
