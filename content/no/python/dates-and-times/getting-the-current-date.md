---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:34.463824-07:00
description: "Hvordan: **Bruke standardbiblioteket `datetime`:** `Datetime`-modulen\
  \ i Pythons standardbibliotek tilbyr klasser for manipulering av datoer og\u2026"
lastmod: '2024-04-05T21:53:41.349595-06:00'
model: gpt-4-0125-preview
summary: '**Bruke standardbiblioteket `datetime`:** `Datetime`-modulen i Pythons standardbibliotek
  tilbyr klasser for manipulering av datoer og klokkeslett.'
title: "F\xE5 dagens dato"
weight: 29
---

## Hvordan:
**Bruke standardbiblioteket `datetime`:**

`Datetime`-modulen i Pythons standardbibliotek tilbyr klasser for manipulering av datoer og klokkeslett. For å få dagens dato, kan du bruke `date.today()`-metoden.

```python
from datetime import date

today = date.today()
print(today)  # Utdata: ÅÅÅÅ-MM-DD (f.eks., 2023-04-05)
```

**Tidsformatering:**

Hvis du trenger dagens dato i et annet format, tillater `strftime`-metoden deg å spesifisere tilpasset datoformatering:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # Eksempelformat: "April 05, 2023"
print(formatted_date)
```

**Bruke `pendulum` for mer fleksibilitet (et populært tredjepartsbibliotek):**

`Pendulum` er et tredjepartsbibliotek som tilbyr en mer intuitiv tilnærming til å håndtere datoer og klokkeslett i Python. Det utvider standard datetime-funksjonaliteten og forenkler håndtering av tidssoner, blant andre funksjoner.

Først, sørg for at du har installert `pendulum` via pip:

```shell
pip install pendulum
```

Deretter, for å få dagens dato:

```python
import pendulum

today = pendulum.now().date()
print(today)  # Utdata: ÅÅÅÅ-MM-DD (f.eks., 2023-04-05)
```

Med `pendulum` er formatering også grei og ligner på `strftime`-tilnærmingen:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # Standard format: "Apr 5, 2023"
print(formatted_date)
```
