---
date: 2024-01-20 17:31:58.729666-07:00
description: "Hvordan gj\xF8r man det: Bruk `datetime` biblioteket i Python. For \xE5\
  \ legge til dager, bruk `timedelta` funksjonen."
lastmod: '2024-03-13T22:44:40.374665-06:00'
model: gpt-4-1106-preview
summary: Bruk `datetime` biblioteket i Python.
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## Hvordan gjør man det:
Bruk `datetime` biblioteket i Python. For å legge til dager, bruk `timedelta` funksjonen.

```python
from datetime import datetime, timedelta

# Dagens dato
i_dag = datetime.now()

# Beregne en fremtidig dato, si 10 dager fra nå
fremtidig_dato = i_dag + timedelta(days=10)
print(f"Fremtidig dato: {fremtidig_dato.strftime('%Y-%m-%d')}")

# Beregne en tidligere dato, si 10 dager siden
tidligere_dato = i_dag - timedelta(days=10)
print(f"Tidligere dato: {tidligere_dato.strftime('%Y-%m-%d')}")
```

Output kan se slik ut:
```
Fremtidig dato: 2023-04-18
Tidligere dato: 2023-03-29
```

## Dypdykk
Historisk sett, før `datetime` modulen, måtte utviklere regne med sekunder og konvertere dem til dager, noe som var tungvint og feilutsatt. Noen alternativer til Pythons `datetime` inkluderer tredjepartsbiblioteker som `Arrow` og `Pendulum` som tilbyr enklere syntaks og tilleggsfunksjoner.

Detaljer:
- `datetime` håndterer skuddsekunder og tidsendringer som sommertid.
- Selve `timedelta` funksjonen støtter mer enn dager; det inkluderer minutter, sekunder, og mikrosekunder.
- I systemer som trenger å håndtere gamle datoer (før 1900), må man bruke eksterne biblioteker fordi `datetime` kan ha begrensninger her.

## Se også
- Python's offisielle dokumentasjon om `datetime`: https://docs.python.org/3/library/datetime.html
- Arrow: https://arrow.readthedocs.io
- Pendulum: https://pendulum.eustace.io
- For en dypere forståelse, “Date and Time Representation in Computers” på Stack Overflow: https://stackoverflow.com/questions/12013216/date-and-time-representation-in-computer-systems
