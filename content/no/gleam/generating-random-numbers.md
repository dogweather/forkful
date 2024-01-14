---
title:    "Gleam: Generering av tilfeldige tall"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor generere tilfeldige tall? Det er mange gode grunner til å bruke tilfeldige tall i programmering. Enten det handler om å lage spennende spill, teste programvare eller simulere tilfeldige hendelser, så kan tilfeldige tall være en nyttig funksjon å ha.

## Hvordan

Å generere tilfeldige tall i Gleam er enkelt med standardbiblioteket `random`. Her er et eksempel på å generere et tilfeldig heltall mellom 1 og 10:

```Gleam
import random

random.int(1, 10)
```

For å generere et tilfeldig desimaltall mellom 0 og 1, kan vi bruke `random.float()`:

```Gleam
import random

random.float()
```

En annen nyttig funksjon er `random.shuffle`, som kan brukes til å blande en liste med elementer tilfeldig. Her er et eksempel på å blande en liste med tall:

```Gleam
import random

let tall = [1, 2, 3, 4, 5]

random.shuffle(tall)

// Output: [3, 5, 2, 1, 4]
```

## Dykk Ned

Bak kulisser lager `random`-modulen i Gleam tilfeldige tall ved hjelp av en pseudorandom-nummergenerator. Dette er en algoritme som bruker en startverdi, kalt en "seed", for å produsere en sekvens med tall som oppfører seg tilfeldig. Hvis samme seed brukes, vil den genererte sekvensen være den samme hver gang. Derfor er det viktig å velge en unik seed hver gang hvis du ønsker en annen sekvens av tall.

En annen viktig ting å merke seg er at tilfeldige tall som genereres av en datamaskin egentlig ikke er helt tilfeldige, men følger et mønster som kan forutsies med nok data. Derfor er det viktig å ikke bruke tilfeldige tall til sikkerhetskritiske formål, som kryptografi.

## Se Også

- Gleam offisiell dokumentasjon om `random`-modulen: https://gleam.run/modules/random/latest
- En grundig forklaring av pseudorandom-nummergeneratorer: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- En oversikt over forskjellige bruksområder for tilfeldige tall: https://www.stat.berkeley.edu/~aldous/157/Papers/random.html