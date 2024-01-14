---
title:    "Python: Utregning av datoer i fremtiden eller fortiden"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å ønske å beregne en dato i fremtiden eller fortiden i et Python-program. Kanskje du planlegger en hendelse og trenger å vite hvilken dato den vil falle på, eller kanskje du vil lage en funksjon som beregner alderen til en person basert på fødselsdatoen deres. Uansett hva årsaken er, kan å kunne beregne datoer i Python være en nyttig ferdighet å ha.

## Hvordan gjøre det

Det er enkelt å beregne datoer i fortiden eller fremtiden ved hjelp av Python sin `datetime` -modul. Først må du importere modulen ved å skrive `import datetime` øverst i programmet ditt. Deretter kan du bruke de forskjellige funksjonene og metodene i modulen for å beregne datoen.

For å beregne en dato i fremtiden, kan du bruke `timedelta` -funksjonen. For eksempel, hvis du vil finne ut hvilken dato det vil være 100 dager fra i dag, kan du skrive følgende i Python:

```Python
from datetime import datetime, timedelta

days_in_future = 100
today = datetime.today()
future_date = today + timedelta(days=days_in_future)

print(f"Datoen {days_in_future} dager fra i dag vil være: {future_date}")
```
Dette vil gi følgende utdata:

`Datoen 100 dager fra i dag vil være: 2021-05-17 17:36:10.044948`

For å beregne en dato i fortiden, kan du bruke `replace` -metoden. Hvis du for eksempel vil finne ut hvilken dato det var for én måned siden, kan du bruke følgende kode:

```Python
from datetime import datetime

today = datetime.today()
past_date = today.replace(month=today.month-1)

print(f"Datoen én måned tilbake fra i dag var: {past_date}")
```
Dette vil gi følgende utdata:

`Datoen én måned tilbake fra i dag var: 2021-03-17 17:45:49.944064`

## Dypdykk

Det er mange flere funksjoner og metoder i `datetime` -modulen som kan hjelpe deg med å beregne datoer i fortiden eller fremtiden. Du kan også bruke `strftime` -metoden til å formatere datoen på den måten du ønsker det. Utforsk gjerne dokumentasjonen til Python sin `datetime` -modul for å lære mer om disse funksjonene og metodene.

## Se også

- [Dokumentasjon for datetime-modulen fra Python sin offisielle nettside](https://docs.python.org/3/library/datetime.html)
- [En interaktiv guide til å beregne datoer i Python](https://www.programiz.com/python-programming/datetime)