---
title:    "Python: Utskrift av feilsøkingsutdata"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor
Det er mange grunner til å engasjere seg i å skrive ut debug-utdata (debug output) mens man programmerer. Det kan hjelpe til med å finne og løse feil, forstå programmet bedre, og gi en oversikt over hvordan koden utføres.

## Hvordan
Det er enkelt å skrive ut debug-utdata i Python. Det kan gjøres ved å bruke funksjonen `print()` og skrive ut variabler, verdier, og meldinger. For eksempel:

```Python
navn = "Per"
print("Hei " + navn)
```

Dette vil skrive ut "Hei Per" i konsollen når programmet kjøres. Det er også mulig å kombinere informasjon fra flere variabler ved å bruke f-strings, som i følgende eksempel:

```Python
alder = 30
print(f"{navn} er {alder} år gammel.")
```

Dette vil skrive ut "Per er 30 år gammel." Slik kan man enkelt se hva som er tilfelle med variablene og hvordan de endres underveis i programmet.

## Dypdykk
Å skrive ut debug-utdata kan bidra til å forstå hvordan koden utføres. Det kan også avdekke feil og logiske fallgruver som man kanskje ikke hadde oppdaget ellers. Man kan også formatere utdataen for å gjøre det mer leselig, for eksempel ved å bruke `sep` og `end` argumenter i `print()`-funksjonen. Det er også mulig å bruke `logger`-modulen i Python for å få mer detaljert informasjon om feil, som kan være nyttig for å finne og løse problemer i større programmer.

## Se Også
- [Offisiell Python dokumentasjon om `print()`](https://docs.python.org/3/library/functions.html#print)
- [Tutorial om debugging i Python](https://realpython.com/python-debugging-pdb/)
- [Informasjon om logger-modulen i Python](https://www.geeksforgeeks.org/logging-in-python/)