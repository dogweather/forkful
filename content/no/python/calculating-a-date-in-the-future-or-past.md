---
title:    "Python: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge fremover eller å se tilbake på viktige hendelser. For eksempel kan det være en måte å finne ut hvor lang tid det er igjen til en viktig begivenhet, som bursdagen til en venn eller et bryllup.

## Slik gjør du det

For å beregne en dato i fremtiden eller fortiden, kan vi bruke Python-modulen datetime. Først må vi importere denne modulen ved å skrive følgende kode:

```Python
import datetime
```

Deretter kan vi bruke funksjonen datetime.now() for å få dagens dato og tid. For å beregne en dato i fremtiden, kan vi bruke funksjonen timedelta som tar i bruk antall dager vi vil legge til eller trekke fra. For eksempel, hvis vi vil beregne en dato 30 dager fra i dag, kan vi skrive:

```Python
future_date = datetime.datetime.now() + datetime.timedelta(days=30)
print(future_date)
```
Dette vil gi oss en output som ligner på dette:

```
2020-08-27 21:00:00.0000
```

For å beregne en dato i fortiden, kan vi bruke samme funksjon, men med et negativt antall dager. For eksempel, hvis vi vil vite hvordan datoen var for 100 dager siden, kan vi skrive:

```Python
past_date = datetime.datetime.now() - datetime.timedelta(days=100)
print(past_date)
```

Output vil da være:

```
2020-04-19 21:00:00.0000
```

## Detaljert forklaring

Beregning av en dato i fortiden eller fremtiden kan være mer komplekst enn bare å legge til eller trekke fra antall dager. Du kan også spesifisere antall timer, minutter eller til og med år. Du kan også kombinere flere variabler for å få mer nøyaktig beregning av en dato. Det er også mulig å bruke ulike formater for datoen, som å få den i en forkortet versjon eller som en tekststreng.

Her er noen nyttige ressurser for å lære mer om hvordan du beregner datoer i Python med datetime-modulen:

- [Python documentation for datetime module](https://docs.python.org/3/library/datetime.html)
- [Real Python article on datetime](https://realpython.com/python-datetime/)
- [Python tutorial on datetime](https://www.programiz.com/python-programming/datetime)

## Se også

- [Python tutorial on formatting dates](https://www.programiz.com/python-programming/datetime/strftime)
- [Python documentation for datetime module](https://docs.python.org/3/library/datetime.html)
- [Real Python article on timedelta](https://realpython.com/python-timedelta/)