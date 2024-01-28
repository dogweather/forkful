---
title:                "Bruke et interaktivt skall (REPL)"
date:                  2024-01-26T04:17:09.593979-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke et interaktivt skall (REPL)"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/using-an-interactive-shell-repl.md"
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
