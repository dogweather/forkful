---
title:                "Python: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge eller forstå hendelser i livet ditt. Det kan også være nyttig for å løse problemer eller forutsi fremtidige datoer for ulike arrangementer.

##Slik gjør du det

```Python
import datetime

# Beregn en dato 100 dager fra i dag
dato = datetime.date.today() + datetime.timedelta(days=100)
print(dato)

# Output:
# 2022-03-31
```

For å beregne en dato i fremtiden, kan du bruke datetime biblioteket i Python. Ved å bruke timedelta funksjonen, kan du legge til et gitt antall dager, uker, måneder eller år til en eksisterende dato. Dette gjør det enkelt å beregne en dato basert på et bestemt antall dager fra dagens dato.

For å beregne en dato i fortiden, kan du bruke samme metode, men i stedet trekke fra et gitt antall dager, uker, måneder eller år til en eksisterende dato. Søk på nettet for å finne ut mer om hvordan du kan bruke datetime biblioteket for å beregne datoer etter dine behov.

##Dypdykk

Datoer kan være komplekse å håndtere, spesielt når man tar hensyn til ulike kalendere og tidszoner. Python har et bredt spekter av biblioteker og moduler som kan hjelpe deg med å håndtere disse situasjonene.

Du kan for eksempel bruke 'dateutil' biblioteket for å håndtere datoer som følger den gregorianske kalenderen, men vises i andre kalendere, som for eksempel den jødiske kalenderen.

Det finnes også biblioteker som 'pytz' som lar deg håndtere tidszoner og konvertere datoer til forskjellige tidssoner. Dette er spesielt nyttig når du må samhandle med systemer som befinner seg i ulike deler av verden.

Søk på nettet for å finne ut mer om disse og andre biblioteker som kan hjelpe deg med å håndtere datoer på en mer avansert måte.

##Se også

- [Python datetime bibliotek dokumentasjon](https://docs.python.org/3/library/datetime.html)
- [dateutil bibliotek dokumentasjon](https://dateutil.readthedocs.io/en/stable/)
- [pytz bibliotek dokumentasjon](http://pytz.sourceforge.net/)