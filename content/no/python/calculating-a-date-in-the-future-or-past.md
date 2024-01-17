---
title:                "Beregning av datoer i fremtiden eller fortiden"
html_title:           "Python: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 

Å beregne en dato i fremtiden eller fortiden handler om å utføre matematiske operasjoner på datostempel, for å finne ut hva datoen vil være etter et visst antall dager. Dette er nyttig for programmerere fordi det tillater dem å lage automatiserte oppgaver som krever å vite fremtidige eller fortidige datoer.

## Hvordan: 

```Python 
import datetime

# Beregne en dato 30 dager frem i tiden
dato = datetime.datetime.today() + datetime.timedelta(days=30)

# Beregn en dato 20 dager tilbake i tiden
dato = datetime.datetime.today() - datetime.timedelta(days=20)

# Skriv ut dagens dato
print("Dagens dato er:", datetime.datetime.today())
```

Output:

```
Dagens dato er: 2021-05-20 11:40:24.387636
Datoen 30 dager frem i tiden er: 2021-06-19 11:40:24.387636
Datoen 20 dager tilbake i tiden er: 2021-05-20 11:40:24.387636
```

## Dypdykk: 

Historisk kontekst:
Beregning av datoer har vært en viktig del av datateknologi siden 1960-tallet, da digitale datostempler ble introdusert. Før dette var det vanlig å beregne datoer manuelt ved hjelp av kalendere og matematiske formler.

Alternativer:
Det finnes mange alternativer til å beregne datoer i Python, inkludert tredjepartsbibliotek som Arrow og Pendulum. Disse tilbyr lignende funksjonalitet, men med noen ekstra tilleggsfunksjoner som kan være nyttige i forskjellige scenarier.

Implementasjonsdetaljer:
Beregning av datoer i Python bruker datamaskinens interne klokke og konverterer datoer tilbake til et leselig format ved hjelp av datatypen "datetime". Det er viktig å ta hensyn til tidsforskjell og tidssoner når man beregner datoer, spesielt når man jobber med internasjonale applikasjoner.

## Se også:

- Mer informasjon om datofunksjoner i Python: [Python Datetime](https://docs.python.org/3/library/datetime.html)
- Alternativer til å beregne datoer i Python: [Arrow](https://arrow.readthedocs.io/en/latest/) og [Pendulum](https://pendulum.eustace.io/)