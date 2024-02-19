---
aliases:
- /no/python/using-an-interactive-shell-repl/
date: 2024-01-26 04:17:09.593979-07:00
description: "En REPL, eller Lese-Evaluer-Skriv-L\xF8kke, er et programmeringsmilj\xF8\
  \ som tar enkeltbrukerinnskrivelser, utf\xF8rer dem og returnerer resultatet til\
  \ brukeren.\u2026"
lastmod: 2024-02-18 23:08:53.525985
model: gpt-4-0125-preview
summary: "En REPL, eller Lese-Evaluer-Skriv-L\xF8kke, er et programmeringsmilj\xF8\
  \ som tar enkeltbrukerinnskrivelser, utf\xF8rer dem og returnerer resultatet til\
  \ brukeren.\u2026"
title: Bruke et interaktivt skall (REPL)
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En REPL, eller Lese-Evaluer-Skriv-Løkke, er et programmeringsmiljø som tar enkeltbrukerinnskrivelser, utfører dem og returnerer resultatet til brukeren. Programmerere bruker det til raske tester, læring, feilsøking eller for å gjøre beregninger på sparket.

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
