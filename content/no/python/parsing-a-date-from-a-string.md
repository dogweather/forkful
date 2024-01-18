---
title:                "Konvertering av en dato fra en streng."
html_title:           "Python: Konvertering av en dato fra en streng."
simple_title:         "Konvertering av en dato fra en streng."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av en dato fra en streng er en vanlig oppgave for Python-programmer. Det innebærer å konvertere en streng som representerer en dato til et datobjekt i Python, som kan manipuleres og brukes til å utføre forskjellige operasjoner. Å kunne parse datoer er spesielt nyttig når man jobber med store datasett eller analyserer tidsserie data.

## Slik gjør du det:

```Python
from datetime import datetime

date_string = '2020-05-15'
parsed_date = datetime.strptime(date_string, '%Y-%m-%d')
print(parsed_date)
```

Output:
```
2020-05-15 00:00:00
```

For å parse en dato fra en streng i Python, bruker vi datetime-modulen som tilbyr forskjellige funksjoner for å håndtere datoer og tid. Funksjonen `strptime()` tar to argumenter - dato-strengen og et format for hvordan datoen er skrevet i strengen. I dette eksemplet bruker vi `%Y` for å indikere fire siffer for år, `%m` for måned og `%d` for dag.

En annen måte å gjøre dette på er å bruke den praktiske `dateutil`-modulen. Denne modulen kan automatisk gjenkjenne datoformater og parse dem uten at man trenger å spesifisere et format.

## Dykk dypere:

Parsing av datoer fra en streng går tilbake til de tidlige utgavene av Python hvor den nå deprecated `time.strftime()`-funksjonen ble brukt. I dag er `datetime`-modulen standard for å håndtere datoer og tid i Python.

Det finnes også tredjeparts biblioteker som `dateparser` og `pandas` som tilbyr mer fleksibilitet og funksjoner for å håndtere datoer og tid.

## Se også:

- [Python datetime-modul dokumentasjon](https://docs.python.org/3/library/datetime.html)
- [dateutil-modulen dokumentasjon](https://dateutil.readthedocs.io/en/stable/)
- [dateparser biblioteket](https://dateparser.readthedocs.io/en/latest/index.html)
- [pandas biblioteket](https://pandas.pydata.org/)