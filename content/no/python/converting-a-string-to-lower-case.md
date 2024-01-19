---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Title: "Konvertere en streng til små bokstaver i Python-programmering"

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver er prosessen der vi endrer alle bokstavene i en streng til små bokstaver. Dette gjøres ofte for å normalisere og sammenligne strenger, eller å fjerne eventuelle forskjeller på grunn av brev tilfelle. 

## Hvordan gjøre det:
Python gjør denne konverteringen veldig enkelt, takket være den innebygde lower()-funksjonen. Her er et eksempel:

```Python
tekst = "Hei Verden!"
print(tekst.lower())
```

Dette vil gi følgende output:

```
hei verden!
```

## Dypdykk
- Historisk kontekst: Python has had the ability to convert strings to lower case since its inception. Having this built-in function allows for rapid and efficient text manipulation, a common need in many programming tasks.

- Alternativer: If you are using pandas for data manipulation, you can use the str.lower() function to convert all the string values of a Series or DataFrame to lower case. 

```Python
import pandas as pd

data = pd.Series(['Hei Verden!', 'Python er Gøy'])
print(data.str.lower())
```
- Implementeringsdetaljer: Python's lower() function doesn't modify the original string; instead, it returns a new string all the characters converted to lower case. This is because strings in Python are immutable.

## Se Også:
For mer informasjon, sjekk ut disse nyttige ressursene:
- Python-doc: [https://docs.python.org/3/library/stdtypes.html#str.lower](https://docs.python.org/3/library/stdtypes.html#str.lower)
- Panda's str.lower(): [https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.str.lower.html](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.Series.str.lower.html)