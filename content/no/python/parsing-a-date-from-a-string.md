---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:38:14.080456-07:00
simple_title:         "Tolke en dato fra en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Omforme en tekststreng til et datobjekt betyr å tolke og konvertere strengen til noe Python forstår som en dato. Vi gjør dette for å manipulere datoer, sammenligne dem, eller lagre dem i en standardisert format.

## How to:
Python bruker `datetime` modulen for å parse datoer. La oss forsøke med `strptime` funksjonen.

```Python
from datetime import datetime

dato_streng = "2023-04-02"
dato_objekt = datetime.strptime(dato_streng, "%Y-%m-%d")
print(dato_objekt)  # Output: 2023-04-02 00:00:00
```

Om vi har en annen format:

```Python
dato_streng_2 = "02/04/2023"
dato_objekt_2 = datetime.strptime(dato_streng_2, "%d/%m/%Y")
print(dato_objekt_2)  # Output: 2023-04-02 00:00:00
```

## Deep Dive
Parsing av datoer ble viktig da datainnsamling og -behandling skjøt fart. Historisk sett kunne formater variere mye, noe som førte til forvirring. Python's `datetime` modul gir en standardisert måte å håndtere dette på.

Alternative metoder kan inkludere bruk av bibliotek som `dateutil`, som har en mer fleksibel parser. For eksempel kan den gjenkjenne og parse flere datoformater uten spesifikk formateringsstreng.

Implementasjonsdetaljer inkluderer behovet for å forstå `strftime`-direktiver for å definere datoformatet. Feil formateringsstreng kan føre til ValueError, så det er viktig med nøyaktighet.

## See Also
- Python's dokumentasjon om `datetime`: https://docs.python.org/3/library/datetime.html
- `dateutil`'s dokumentasjon: https://dateutil.readthedocs.io/en/stable/
- Python's strftime referanse: https://strftime.org/
