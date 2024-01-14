---
title:    "Python: Finner lengden av en streng"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor
Å finne lengden på en streng i et programmeringsspråk som Python kan virke som en enkel oppgave, men det kan være en svært nyttig ferdighet å ha. Det vil tillate deg å manipulere og håndtere tekstdatatype på en mer effektiv måte, noe som er uunværlig for å lage programmer og løse problemer.

## Hvordan
Det finnes flere måter å finne lengden på en streng i Python på, men den enkleste metoden er å bruke den innebygde funksjonen `len()`. La oss se på et eksempel:

```Python
streng = "Dette er en streng"
print(len(streng))
```
Output: 18

I dette eksempelet har vi definert en variabel `streng` som inneholder en string-verdi. Deretter bruker vi `len()`-funksjonen til å finne antall tegn i strengen og skriver det ut ved hjelp av `print()`.

En annen måte å finne lengden på en streng på, er å bruke en for-løkke. Her er et eksempel på hvordan det kan gjøres:

```Python
streng = "Dette er en streng"
counter = 0
for c in streng:
    counter += 1
print(counter)
```
Output: 18

Vi har fortsatt den samme strengen som før, men denne gangen bruker vi en for-løkke for å telle antall tegn i strengen. Variabelen `counter` øker med 1 for hvert tegn i strengen.

## Dypdykk
Nå som vi har sett på to måter å finne lengden på en streng i Python på, la oss dykke litt dypere inn i hvordan dette egentlig fungerer. Når vi bruker `len()`-funksjonen, blir den egentlig bare oversatt til å kalle `.__len__()`-metoden på objektet, som er implementert av standard databehandlere for ulike datatyper.

Denne metoden returnerer lengden på objektet, og i tilfelle med en streng, vil den returnere antall tegn i strengen. Dette er en rask og effektiv måte å finne lengden på en streng på, og derfor bør det være den foretrukne metoden.

## Se også
- [Offisiell Python dokumentasjon om len()](https://docs.python.org/3/library/functions.html#len)
- [En guide til grunnleggende strenger i Python](https://www.datacamp.com/community/tutorials/python-string-tutorial)
- [10 tips for å lære å kode i Python](https://medium.com/better-programming/10-tips-to-learn-to-code-in-python-bd80ff004eee)