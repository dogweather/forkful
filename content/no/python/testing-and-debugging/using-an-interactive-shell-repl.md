---
date: 2024-01-26 04:17:09.593979-07:00
description: "Hvordan: G\xE5 rett inn i Pythons REPL ved \xE5 skrive `python` i kommandolinjen\
  \ din. N\xE5r du er der, test ut enkle operasjoner eller flerlinjekode."
lastmod: '2024-03-13T22:44:40.362995-06:00'
model: gpt-4-0125-preview
summary: "G\xE5 rett inn i Pythons REPL ved \xE5 skrive `python` i kommandolinjen\
  \ din."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

## Hvordan:
Gå rett inn i Pythons REPL ved å skrive `python` i kommandolinjen din. Når du er der, test ut enkle operasjoner eller flerlinjekode:

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

Eksperimentér med funksjoner og umiddelbar tilbakemelding:

```Python
>>> def greet(name):
...     return "Hei, " + name + "!"
... 
>>> greet("Alice")
'Hei, Alice!'
```

Lek med biblioteker og utforsk funksjonene deres i sanntid:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

Avslutt med et kjapt `exit()` eller `Ctrl+D` (noen ganger `Ctrl+Z` på Windows).

## Dypdykk
Konseptet med en REPL er ikke unikt for Python; det er like gammelt som Lisp. Mange språk tilbyr dette umiddelbare, interaktive miljøet for en praktisk tilnærming til koding. Alternativer til det innfødte Python-skalet inkluderer IPython og Jupyter Notebook, som gir forbedret interaktivitet, flere funksjoner og bedre integrasjon med andre verktøy. Pythons standard REPL er enkel, men den inneholder hele kraften til Python, håndterer komplekse objekter og flertrådede programmer, selv om den mangler funksjoner som autofullføring og syntaksfremheving som er til stede i mer avanserte verktøy.

## Se også
- [Pythons offisielle dokumentasjon om tolken](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: En avansert Python-skal](https://ipython.org/)
- [Jupyter-prosjektet](https://jupyter.org/)
