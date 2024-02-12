---
title:                "Använda en interaktiv skal (REPL)"
aliases:
- /sv/python/using-an-interactive-shell-repl/
date:                  2024-01-26T04:17:11.879919-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En REPL, eller Läs-Utvärdera-Skriv Loop, är en programmeringsmiljö som tar emot enskilda användarinput, exekverar dem och returnerar resultatet till användaren. Programmerare använder den för snabba tester, lärande, felsökning eller att göra beräkningar på flygande fot.

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
