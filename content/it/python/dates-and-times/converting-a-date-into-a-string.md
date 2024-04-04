---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "Come fare: Python semplifica la conversione delle date in stringhe.\
  \ Utilizza il metodo\u2026"
lastmod: '2024-04-04T02:02:43.200435-06:00'
model: gpt-4-0125-preview
summary: Python semplifica la conversione delle date in stringhe.
title: Convertire una data in una stringa
weight: 28
---

## Come fare:
Python semplifica la conversione delle date in stringhe. Utilizza il metodo [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) disponibile sugli oggetti [date](https://docs.python.org/3/library/datetime.html#date-objects). Ecco come:

```Python
from datetime import datetime

# Ottieni la data e l'ora corrente
now = datetime.now()

# Convertila in una stringa nel formato: Mese giorno, Anno
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Output: Marzo 29, 2023 (o data corrente)

# Formato: AAAA-MM-GG
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Output: 2023-03-29 (o data corrente)
```

### Come lo faccio

Ecco come ottengo una data in formato [ISO 8601](https://www.w3.org/QA/Tips/iso-date) con informazioni sul fuso orario:

```python
def datestamp() -> str:
    """ 
    La data e l'ora corrente con fuso orario in formato ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### Esempio di output:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```

## Approfondimento
Storicamente, la conversione da data a stringa è stata un pilastro nella programmazione a causa della necessità di rappresentare le date in un formato leggibile dall'uomo.

Alternative a `strftime` includono l'utilizzo del metodo `isoformat` per il formato ISO 8601, o librerie di terze parti come `arrow` e `dateutil` che offrono opzioni di parsing e formattazione più flessibili.

Dal punto di vista implementativo, `strftime` sta per "string format time" ed ha le sue radici nella programmazione C. `strftime` di Python interpreta codici di formato come `%Y` per l'anno e `%m` per il mese, permettendo una personalizzazione quasi infinita.

## Vedi anche
Per approfondire le funzioni di data e ora di Python:
- Documentazione ufficiale di `datetime` di Python: https://docs.python.org/3/library/datetime.html
- Per chi è interessato a una lista completa delle direttive `strftime`: https://strftime.org/
- Per esplorare librerie di data/ora di terze parti:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
