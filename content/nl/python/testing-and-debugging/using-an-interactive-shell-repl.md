---
title:                "Het gebruik van een interactieve shell (REPL)"
date:                  2024-01-28T22:09:30.353687-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een REPL, of Read-Eval-Print Loop, is een programmeeromgeving die individuele gebruikersinvoer neemt, deze uitvoert, en het resultaat aan de gebruiker teruggeeft. Programmeurs gebruiken het voor snelle tests, leren, debuggen of het doen van berekeningen ter plekke.

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
