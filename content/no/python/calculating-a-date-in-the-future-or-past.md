---
title:                "Python: Beregning av en dato i fremtiden eller fortiden"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

### Hvorfor
Det kan være mange grunner til at man ønsker å kunne beregne en dato i fremtiden eller fortiden i Python. Dette kan være nyttig for å planlegge fremtidige hendelser, beregne alder eller for å løse mer komplekse problemstillinger.

### Hvordan
For å beregne en dato i Python, må man først importere datatypen datetime ved å skrive følgende kode:

```Python
import datetime
```

Deretter kan man initialisere en variabel med dagens dato ved å bruke datatypen datetime.date som vist under:

```Python
today = datetime.date.today()
```

For å beregne en dato i fremtiden er det bare å legge til ønsket antall dager til variabelen today. For eksempel, hvis vi ønsker å finne datoen 30 dager frem i tid kan vi gjøre følgende:

```Python
future_date = today + datetime.timedelta(days=30)
```

På samme måte kan man beregne en dato i fortiden ved å trekke fra ønsket antall dager. Her er et eksempel på å beregne datoen 30 dager tilbake i tid:

```Python
past_date = today - datetime.timedelta(days=30)
```

Det er også mulig å beregne datoer utifra andre enheter, som for eksempel uker, år eller timer. For å lære mer om dette kan man lese dokumentasjonen til datatypen datetime.

### Dype dykk
I tillegg til å kunne beregne en enkelt dato, kan man også gjøre mer avanserte beregninger i Python. Dette kan inkludere å finne ut hvilken dag en dato faller på, beregne forskjellen mellom to datoer eller manipulere datoer basert på spesifikke krav. Det finnes også flere biblioteker som kan hjelpe med mer komplekse beregninger, som for eksempel Arrow eller Pendulum.

### Se også
- [Dokumentasjon for datatypen datetime i Python](https://docs.python.org/3/library/datetime.html)
- [Arrow biblioteket for dato og tid håndtering i Python](https://arrow.readthedocs.io/en/latest/)
- [Pendulum biblioteket for enkel dato og tid håndtering i Python](https://pendulum.eustace.io/docs/)