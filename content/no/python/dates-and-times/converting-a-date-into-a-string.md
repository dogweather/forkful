---
title:                "Konvertere en dato til en streng"
aliases:
- /no/python/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:12.590511-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en dato til en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av dato til streng handler om å endre et datoobjekt til en tekstrepresentasjon. Dette er nyttig for å vise datoer lesevennlig for brukere eller lagre i databaser som ikke støtter datoobjekter.

## Hvordan gjøre det:
```Python
from datetime import datetime

# Nåværende dato og tid
naa = datetime.now()

# Konverter til streng
dato_streng = naa.strftime("%d.%m.%Y %H:%M")
print(dato_streng)
```
Eksempelutskrift:
```
24.03.2023 15:45
```

## Dypdykk
Historisk har datoer og tidspunkt i programmering ofte vært representert ved tall eller komplekse strukturer, som i C's `tm` struktur. Python tilbyr derimot `datetime`-modulen, noe som forenkler arbeidet med datoer og tider.

Det finnes flere måter å konvertere datoer til strenger på i Python. `strftime()` er den vanligste funksjonen og står for "string format time". Den lar deg definere hvordan dato og tid skal formateres med spesifikke koder, som `%d` for dag og `%m` for måned. Andre metoder inkluderer å bruke ISO-format ved `isoformat()` eller konvertere til timestamp og så til streng.

En ting å vokte seg for er tidsonebehandling – `datetime.now()` uten argument gir tiden i lokal tidssone. Ønsker man å sikre universell tid, kan `datetime.utcnow()` brukes.

## Se også
- Python `datetime`-modulen: https://docs.python.org/3/library/datetime.html
- strftime() og strptime() adferd: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior
- Tidssoner i Python med pytz: https://pypi.org/project/pytz/
