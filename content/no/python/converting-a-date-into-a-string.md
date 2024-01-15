---
title:                "Omforming av dato til streng"
html_title:           "Python: Omforming av dato til streng"
simple_title:         "Omforming av dato til streng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor vil du konvertere et datoobjekt til en tekststreng? Det kan være nyttig når du vil vise datoen på en mer leselig måte, for eksempel på en nettside eller i en logger.

## Hvordan

Konvertering av dato til streng i Python er enkelt og kan gjøres på flere måter. Her er et eksempel på bruk av `strftime()`-metoden, som tar inn en dato og formaterer den som en tekststreng:

```Python
from datetime import datetime
date = datetime(2021, 11, 3)
print(date.strftime("%d/%m/%Y")) # output: 03/11/2021
```

Du kan også bruke `str()`-funksjonen for å konvertere et datoobjekt til en streng:

```Python
from datetime import date
today = date.today()
print(str(today)) # output: 2021-11-03
```

Det er viktig å merke seg at utgangsformatet for dato kan variere avhengig av operativsystemet og lokaliseringsinnstillingene dine.

## Dypdykk

Når du bruker `strftime()`-metoden for å formatere datoer, må du bruke spesifikke symboler i strengformatet for å få riktig utgang. For eksempel betyr `%d` dag i måneden, `%m` måned i året og `%Y` året med fullt tall.

Det er også mulig å bruke `datetime`-modulen til å konvertere en tekststreng til et datoobjekt. Dette kan være nyttig når du for eksempel leser inn datoer fra en fil eller brukerinput:

```Python
from datetime import datetime
date = datetime.strptime("03-11-2021", "%d-%m-%Y")
print(date) # output: 2021-11-03 00:00:00
```

## Se også

- [Dokumentasjon for `datetime`-modulen](https://docs.python.org/3/library/datetime.html)
- [Sololearn Python-kurs: Dato og tid](https://www.sololearn.com/Course/Python/?ref=app)