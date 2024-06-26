---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:38.766158-07:00
description: "Hur: **Anv\xE4nda standardbiblioteket `datetime`:** `datetime`-modulen\
  \ i Pythons standardbibliotek tillhandah\xE5ller klasser f\xF6r att manipulera datum\
  \ och\u2026"
lastmod: '2024-04-05T21:53:38.820996-06:00'
model: gpt-4-0125-preview
summary: "**Anv\xE4nda standardbiblioteket `datetime`:** `datetime`-modulen i Pythons\
  \ standardbibliotek tillhandah\xE5ller klasser f\xF6r att manipulera datum och tider."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Hur:
**Använda standardbiblioteket `datetime`:**

`datetime`-modulen i Pythons standardbibliotek tillhandahåller klasser för att manipulera datum och tider. För att få det aktuella datumet kan du använda metoden `date.today()`.

```python
from datetime import date

idag = date.today()
print(idag)  # Utdata: ÅÅÅÅ-MM-DD (t.ex., 2023-04-05)
```

**Tidsformatering:**

Om du behöver aktuellt datum i ett annat format, låter metoden `strftime` dig specificera anpassad datumformatering:

```python
from datetime import date

idag = date.today()
formaterat_datum = idag.strftime('%B %d, %Y')  # Exempelformat: "April 05, 2023"
print(formaterat_datum)
```

**Använda `pendulum` för mer flexibilitet (ett populärt tredjepartbibliotek):**

`Pendulum` är ett tredjepartbibliotek som erbjuder en mer intuitiv metod för att hantera datum och tider i Python. Det utökar standard datetime-funktionaliteter och förenklar hantering av tidszoner, bland andra funktioner.

Först, se till att du har installerat `pendulum` via pip:

```shell
pip install pendulum
```

Sedan, för att få det aktuella datumet:

```python
import pendulum

idag = pendulum.now().date()
print(idag)  # Utdata: ÅÅÅÅ-MM-DD (t.ex., 2023-04-05)
```

Med `pendulum` är även formatering okomplicerad och liknar `strftime`-metoden:

```python
import pendulum

idag = pendulum.now()
formaterat_datum = idag.to_formatted_date_string()  # Standardformat: "Apr 5, 2023"
print(formaterat_datum)
```
