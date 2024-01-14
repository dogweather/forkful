---
title:    "Python: Generering av tilfeldige tall"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i programmering, trenger vi å generere tilfeldige tall. Dette kan være for å simulere ulike situasjoner eller for å lage tilfeldige verdier for tester. Uansett årsak, kan det å ha kunnskap om hvordan man genererer tilfeldige tall være svært nyttig for en programmerer.

## Slik gjør du det

Det er flere måter å generere tilfeldige tall i Python, men den mest vanlige metoden er å bruke "random" biblioteket. For å bruke dette biblioteket, må du først importere det i koden din ved å skrive:

```Python
import random
```

Deretter kan du bruke funksjonen "random.randint(a, b)" for å generere et tilfeldig heltall mellom a og b, inkludert a og b. For eksempel, hvis du ønsker å generere et tilfeldig heltall mellom 1 og 10, kan du skrive:

```Python
random.randint(1, 10)
```

Hvis du ønsker å generere et tilfeldig desimaltall mellom 0 og 1, kan du bruke "random.random()" funksjonen. Denne funksjonen vil generere et desimaltall mellom 0 og 1, ikke inkludert 1.

```Python
random.random()
```

Det er også mulig å bruke "random.uniform(a, b)" funksjonen for å generere et tilfeldig desimaltall mellom a og b, inkludert a og b.

For å se hvordan tilfeldige tall kan genereres, kan du prøve å kjøre følgende kode:

```Python
import random

# Generer et tilfeldig heltall mellom 1 og 100
random.randint(1, 100)

# Generer et tilfeldig desimaltall mellom 0 og 1
random.random()

# Generer et tilfeldig desimaltall mellom 5 og 10
random.uniform(5, 10)
```

Outputet av denne koden kan se slik ut:

```
>>> 56
>>> 0.7895310639626686
>>> 7.193560328242549
```

## Dypdykk

Mens det å bruke "random" biblioteket er den vanligste måten å generere tilfeldige tall på, er det også andre måter å gjøre det på. For eksempel, om du ønsker å generere tilfeldige tall fra en bestemt distribusjon, kan du bruke NumPy biblioteket. Dette biblioteket tilbyr en rekke funksjoner for å generere tilfeldige tall fra ulike distribusjoner som normalfordeling og eksponentialfordeling.

En annen viktig ting å vite er at "random" biblioteket egentlig ikke genererer helt tilfeldige tall, men heller "pseudo-tilfeldige" tall. Dette betyr at tallene er generert ut fra en algoritme og en startverdi, og vil følge et forutsigbart mønster. Dette kan være nyttig å vite om du ønsker å generere de samme tallene flere ganger.

## Se også

- [Dokumentasjon for "random" biblioteket i Python](https://docs.python.org/3/library/random.html)
- [NumPy dokumentasjon for å generere tilfeldige tall](https://numpy.org/doc/stable/reference/random/index.html)