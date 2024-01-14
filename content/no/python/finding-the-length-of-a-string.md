---
title:    "Python: Finn lengden av en streng"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en tekststreng er en grunnleggende og nyttig ferdighet for alle som lærer å programmere. Det å være i stand til å finne lengden på en streng kan hjelpe deg med å håndtere og bearbeide data på en mer effektiv måte.

## Hvordan

For å finne lengden på en streng i Python, bruker vi funksjonen "len()". Denne funksjonen tar en streng som argument og gir tilbake lengden på strengen.

```Python
streng = 'Hei, dette er en streng!'
print(len(streng))
```
**Output**
```
24
```
Som du kan se, gir funksjonen "len()" oss lengden på strengen, som er 24 i dette tilfellet. Vi kan også bruke denne funksjonen på variabler som inneholder strenger.

```Python
navn = 'Johanna'
print(len(navn))
```
**Output**
```
7
```

En annen måte å finne lengden på en streng på er ved å bruke en løkke og telle antall tegn i strengen, men dette er ikke like effektivt som å bruke funksjonen "len()".

## Dypdykk

Det kan være nyttig å vite at funksjonen "len()" ikke bare fungerer på strenger, men også på andre datatyper som lister og tupler. Den gir tilbake antall elementer i en liste eller tuple.

```Python
liste = [1, 2, 3, 4, 5]
print(len(liste))

tuple = (6, 7, 8, 9, 10)
print(len(tuple))
```
**Output**
```
5
5
```

I tillegg kan "len()" også brukes på mer komplekse objekter som dictionaries, der den gir tilbake antall nøkler i dictionaryet.

Et viktig aspekt ved å finne lengden på en streng er å forstå at det er forskjell på antall tegn og antall bytes i en streng. Dette er spesielt viktig når man jobber med flerspråklige programmer, der forskjellige språk har ulike størrelser på bokstaver og tegn.

## Se Også

- [Python dokumentasjon om `len()` funksjonen](https://docs.python.org/no/3/library/functions.html#len)
- [En grundig guide om å finne lengden på en streng i Python](https://www.programiz.com/python-programming/methods/built-in/len)