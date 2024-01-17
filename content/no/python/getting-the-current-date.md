---
title:                "Hente nåværende dato"
html_title:           "Python: Hente nåværende dato"
simple_title:         "Hente nåværende dato"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hva er og hvorfor bruker vi den nåværende datoen?

Å få nåværende dato er et vanlig behov for mange programmerere, enten det er å spore tid, lage tidsstempler eller utføre beregninger basert på datoen. Det er viktig for programmerere å ha nøyaktig informasjon om tiden for å sikre at deres koder fungerer riktig og effektivt.

# Hvordan:

```Python

# Importer datetime modulen
import datetime

# Bruk datetime.now () -funksjonen for å få nåværende dato og lagre den i en variabel
current_date = datetime.now()

# Skriv ut nåværende dato i dag / måned / år-format
print(current_date.strftime("%d/%m/%Y"))

# Skriv ut nåværende dato i dagens navn, månedsnavn, år-format
print(current_date.strftime("%A, %B %Y"))

```

```
Output:

19/11/2021
Friday, November 2021
```

# Dykk dypere:

Det finnes flere måter å få nåværende dato på i Python, som å bruke time-modulen eller calendar-modulen. Du kan også spesifisere tidssone, formatere dato og til og med utføre matematiske operasjoner med datoer ved hjelp av datetime-modulen.

# Se også:

- Python dokumentasjon for datetime-modulen: https://docs.python.org/3/library/datetime.html
- Tutorialspoint sin artikkel om hvordan å få nåværende dato i Python: https://www.tutorialspoint.com/python/time_strftime.htm