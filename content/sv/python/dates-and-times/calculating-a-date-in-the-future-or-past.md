---
aliases:
- /sv/python/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:32:04.255597-07:00
description: "Datumber\xE4kningar \xE4r typ best\xE4mning av framtida eller f\xF6\
  rg\xE5ngna datum fr\xE5n en given punkt. Programmerare g\xF6r det f\xF6r att hantera\
  \ bokningar, p\xE5minnelser,\u2026"
lastmod: 2024-02-18 23:08:51.434567
model: gpt-4-1106-preview
summary: "Datumber\xE4kningar \xE4r typ best\xE4mning av framtida eller f\xF6rg\xE5\
  ngna datum fr\xE5n en given punkt. Programmerare g\xF6r det f\xF6r att hantera bokningar,\
  \ p\xE5minnelser,\u2026"
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
---

{{< edit_this_page >}}

## Vad & Varför?
Datumberäkningar är typ bestämning av framtida eller förgångna datum från en given punkt. Programmerare gör det för att hantera bokningar, påminnelser, historiska data och tidsbaserade algoritmer.

## Hur gör man:
Python har inbyggda verktyg för datumhantering. `datetime` är stjärnan. Här är hur man gör:

```python
from datetime import datetime, timedelta

# Nuvarande datum och tid
nu = datetime.now()

# Beräkna ett datum i framtiden
framtida_datum = nu + timedelta(days=10)  # +10 dagar från nu
print(f"Framtida datum: {framtida_datum}")

# Beräkna ett datum i det förflutna
forflutet_datum = nu - timedelta(weeks=3)  # -3 veckor från nu
print(f"Förgånget datum: {forflutet_datum}")
```

Förväntad output:
```
Framtida datum: 2023-04-22 14:15:38.686528
Förgånget datum: 2023-03-11 14:15:38.686528
```

## Djupdykning:
I det förflutna (förr i tiden, va?), folks datumberäkningar var manuella—miniräknare och almanackor. Men med Pythons `datetime`, sker allt automatiskt. Alternativ till `datetime` inkluderar `dateutil`, `arrow`, och `Pendulum` som erbjuder mer flexibilitet eller bekvämligheter.

För att räkna datum exakt behövs bearbetning av skottsekunder och tidszoner—`datetime` kan hantera tidszoner medan externa bibliotek såsom `pytz` och `zoneinfo` (från Python 3.9) kan vara nödvändiga för mer komplexa scenario.

## Se även:
- Python `datetime` dokumentation: https://docs.python.org/3/library/datetime.html
- `dateutil` biblioteket: https://dateutil.readthedocs.io/en/stable/
- `pytz` tidszonsbiblioteket: http://pytz.sourceforge.net/
- `zoneinfo` modulen: https://docs.python.org/3/library/zoneinfo.html
