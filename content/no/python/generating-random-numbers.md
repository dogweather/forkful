---
title:                "Generering av tilfeldige tall"
html_title:           "Python: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å generere tilfeldige tall er en vanlig praksis blant programmører når de trenger tilfeldige elementer i koden sin. Dette kan være for å lage tilfeldige verdier for testing eller for å lage et variert utvalg av data. Det er også nyttig for å sikre at en algoritme eller program ikke blir forutsigbar og møter uventede situasjoner.

# Hvordan gjør man det?
For å generere tilfeldige tall i Python, kan vi bruke funksjonen ```random()``` fra modulen ```random```. La oss se på et eksempel:

```Python
import random

print(random.random()) 
```

Dette vil gi ut et tilfeldig tall mellom 0 og 1. For å få et tilfeldig tall innenfor et bestemt område, kan vi bruke ```randint()```-funksjonen:

```Python
import random

print(random.randint(1, 10)) 
```

Dette vil gi ut et tilfeldig heltall mellom 1 og 10.

# Dykk ned i det
Historisk sett har generering av tilfeldige tall vært en utfordring for matematikere og programmerere. I tidligere tider brukte man ofte fysiske fenomener som kast av mynter eller terninger for å få tilfeldige tall, men dette var ikke alltid nøyaktig og kunne være tidkrevende. I dag bruker de fleste programmeringsspråk, inkludert Python, algoritmer for å generere tilfeldige tall basert på en startverdi kalt en "seed". Dette gjør det mulig å gjenskape den samme rekkefølgen av tilfeldige tall ved å bruke samme seed, noe som kan være nyttig i testing og debugging.

Et alternativ til å bruke ```random```-modulen i Python er å bruke den dedikerte modulen ```numpy```, som har flere funksjoner for å generere tilfeldige tall med spesifikk fordeling, for eksempel normalfordeling eller jevn fordeling.

# Se også
- [Python Random Modul](https://docs.python.org/3/library/random.html)
- [Numpy Random Modul](https://numpy.org/doc/stable/reference/random/index.html)