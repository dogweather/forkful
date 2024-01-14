---
title:                "Python: Å få nåværende dato"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få tak i dagens dato kan være viktig for å lage dynamiske programmer eller å registrere informasjon på en korrekt måte. Det er også nyttig for å holde oversikt over tidssensitive oppgaver eller å generere rapporter.

## Hvordan

Å få tak i dagens dato i Python er enkelt. Først må vi importere biblioteket "datetime" ved å skrive følgende kode:

```Python
import datetime
```

Deretter kan vi bruke funksjonen "datetime.now()" for å få tak i dagens dato og klokkeslett. La oss se på et eksempel:

```Python
current_date = datetime.now()
print(current_date)
```
Dette vil gi oss følgende output:

```Output
2021-03-18 15:25:00.244533
```

Vi kan også formatere outputen ved å bruke funksjonen "strftime()" og spesifisere ønsket format. Her er et eksempel:

```Python
current_date = datetime.now()
formatted_date = current_date.strftime("%d.%m.%Y")
print(formatted_date)
```

Dette vil gi oss følgende output:

```Output
18.03.2021
```

## Dypdykk

"datetime" biblioteket gir også muligheten til å få tak i spesifikke deler av datoen, som for eksempel dag, måned og år. Dette kan være nyttig hvis vi ønsker å utføre beregninger eller sammenligninger basert på disse verdiene. Her er et eksempel på hvordan vi kan få tak i dagens år:

```Python
current_date = datetime.now()
year = current_date.year
print(year)
```

Dette vil gi oss følgende output:

```Output
2021
```

Vi kan også bruke "timedelta" funksjonen for å gjøre beregninger basert på datoer. For eksempel, hvis vi ønsker å legge til 100 dager til dagens dato, kan vi gjøre følgende:

```Python
current_date = datetime.now()
new_date = current_date + timedelta(days=100)
print(new_date)
```

Dette vil gi oss følgende output:

```Output
2021-06-26 15:40:00.244533
```

## Se Også

- [Python offisiell dokumentasjon for datetime](https://docs.python.org/3/library/datetime.html)
- [RealPython - Manipulating Dates and Times in Python](https://realpython.com/python-datetime/)
- [W3Schools - Python Datetime](https://www.w3schools.com/python/python_datetime.asp)