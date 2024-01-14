---
title:                "Python: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å printe ut debug-output kan være en nyttig måte å feilsøke og forstå koden din på. Det kan hjelpe deg med å identifisere og løse problemer i programmet ditt, og bidra til å forbedre din generelle programmeringskunnskap.

## Hvordan

For å skrive ut debug-output i Python, kan du bruke funksjonen `print()`. Denne funksjonen lar deg skrive ut verdier av variabler eller uttrykk rett i konsollen. La oss se på et eksempel:

```Python
navn = "Marie"
print("Hei, mitt navn er", navn)
```

Dette vil resultere i følgende utskrift til konsollen:

```
Hei, mitt navn er Marie
```

Du kan også legge til flere variabler eller tekster i en `print()`-funksjon ved å bruke komma mellom dem. Dette kan være nyttig når du ønsker å følge med på verdien av flere variabler samtidig.

Debug-output kan også være nyttig når du jobber med løkker eller vil identifisere spesifikke deler av koden din. La oss se på et eksempel med en `while`-løkke:

```Python
tall = 1
while tall < 10:
    print("Tall:", tall)
    tall += 1
```

Dette vil skrive ut tallene fra 1 til 9 i konsollen.

## Dypdykk

Å printe debug-output kan også være nyttig når du ønsker å dele opp koden din og få en bedre forståelse for hvordan den fungerer. Ved å printe ut verdier i forskjellige deler av koden, kan du følge med på hvordan verdiene endrer seg og hvorfor.

En annen måte å bruke debug-output på, er ved å bruke `assert`-setninger. Disse kan hjelpe deg med å teste om forventede verdier stemmer med faktiske verdier i koden din.

## Se også

- [Python Tutorial: Basic Debugging Techniques](https://realpython.com/python-debugging-pdb/)
- [Python Debugging Techniques: Working with Assertions](https://www.youtube.com/watch?v=ZcE4brP6_eI)
- [Debugging in Python: A Simplified Guide](https://stackify.com/python-debugging-tips/)
- [Debugging with print statements in Python](https://www.datacamp.com/community/tutorials/debugging-python-print-statements)