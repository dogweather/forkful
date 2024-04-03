---
date: 2024-01-26 04:17:11.879919-07:00
description: "En REPL, eller L\xE4s-Utv\xE4rdera-Skriv Loop, \xE4r en programmeringsmilj\xF6\
  \ som tar emot enskilda anv\xE4ndarinput, exekverar dem och returnerar resultatet\
  \ till\u2026"
lastmod: '2024-03-13T22:44:37.484751-06:00'
model: gpt-4-0125-preview
summary: "En REPL, eller L\xE4s-Utv\xE4rdera-Skriv Loop, \xE4r en programmeringsmilj\xF6\
  \ som tar emot enskilda anv\xE4ndarinput, exekverar dem och returnerar resultatet\
  \ till anv\xE4ndaren."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur man gör:
Dyk direkt in i Pythons REPL genom att skriva `python` i din kommandotolk. Väl där, testa enkla operationer eller flerlinjekod:

```Python
>>> 1 + 1
2
>>> för i in range(3):
...     print(i)
... 
0
1
2
```

Experimentera med funktioner och omedelbar feedback:

```Python
>>> def hälsa(namn):
...     return "Hej, " + namn + "!"
... 
>>> hälsa("Alice")
'Hej, Alice!'
```

Lek med bibliotek och utforska deras funktioner i realtid:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

Avsluta med ett snabbt `exit()` eller `Ctrl+D` (ibland `Ctrl+Z` på Windows).

## Djupdykning
Konceptet med en REPL är inte unikt för Python; det är lika gammalt som Lisp. Många språk erbjuder denna omedelbara, interaktiva miljö för ett hands-on angreppssätt till kod. Alternativ till den inbyggda Python-tolken inkluderar IPython och Jupyter Notebook, som erbjuder förbättrad interaktivitet, fler funktioner och bättre integration med andra verktyg. Pythons standard REPL är enkel, men den inbäddar hela kraften hos Python, hanterar komplexa objekt och flertrådade program, även om den saknar funktioner som autokomplettering och syntaxmarkering som finns i mer avancerade verktyg.

## Se även
- [Pythons officiella dokumentation om tolken](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: En avancerad Python-tolk](https://ipython.org/)
- [Jupyter Project](https://jupyter.org/)
