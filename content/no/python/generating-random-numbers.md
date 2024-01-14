---
title:    "Python: Generering av tilfeldige tall"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Generering av tilfeldige tall kan være svært nyttig i programmering. Det kan hjelpe deg med å teste ut ulike scenarier og lage varierte datasett.

## Hvordan
For å generere tilfeldige tall i Python, kan du bruke "random" biblioteket. Her er et eksempel på hvordan du kan generere et tilfeldig tall mellom 1 og 10 og skrive det ut:

```Python
import random
print(random.randint(1,10))
```

Dette vil skrive ut et tilfeldig tall hver gang koden kjøres, for eksempel: 7, 3, 9 osv.

Du kan også generere tilfeldige tall fra en liste ved hjelp av "choice" funksjonen. Her er et eksempel på hvordan du kan generere et tilfeldig navn fra en liste og skrive det ut:

```Python
import random
names = ["Ella", "Olav", "Sofia", "William", "Mia"]
print(random.choice(names))
```

Dette vil skrive ut et tilfeldig navn fra listen hver gang koden kjøres, for eksempel: Olav, William, Mia osv.

## Dypdykk
Python har flere funksjoner som kan hjelpe deg med å generere tilfeldige tall på ulike måter. "random.uniform()" kan for eksempel generere tilfeldige desimaltall mellom et gitt intervall. "random.sample()" kan generere en liste med tilfeldige tall eller elementer fra en eksisterende liste uten å duplisere dem. 

Det er også mulig å sette en startverdi, kalt en "seed", for å få de samme tilfeldige tallene hver gang koden kjøres. Dette kan gjøres ved å bruke funksjonen "random.seed()". Dette kan være nyttig hvis du trenger å kunne gjenskape resultatene dine.

## Se også
- [Dokumentasjon for random modulen i Python](https://docs.python.org/3/library/random.html)
- [Generere tilfeldige tall med numpy biblioteket](https://numpy.org/doc/stable/reference/random/generated/numpy.random.randint.html)
- [Hvordan generere tilfeldige tall i andre programmeringsspråk](https://www.geeksforgeeks.org/generating-random-number-list-in-python/)