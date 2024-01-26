---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:16:13.734312-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Ottenere la data corrente permette ai programmi di avere un riferimento temporale attuale. I programmatori lo fanno per tracciare eventi, gestire operazioni cronologiche o semplicemente mostrare la data e l'ora agli utenti.

## Come Fare:
Per ottenere la data corrente in Python, utilizzi il modulo `datetime`. Ecco come si fa:

```Python
from datetime import datetime

# Ottieni la data e l'ora correnti
ora_corrente = datetime.now()

print(f"Data e ora correnti: {ora_corrente}")
```

Output:
```
Data e ora correnti: 2023-04-05 14:23:01.123456
```

Solo la data? Nessun problema:

```Python
# Ottieni solo la data corrente
data_corrente = datetime.now().date()

print(f"Data corrente: {data_corrente}")
```

Output:
```
Data corrente: 2023-04-05
```

## Approfondimento
Prima degli anni '80, ottenere la data e l'ora in programmazione non era standardizzato, e spesso si basava su funzioni specifiche del sistema operativo. Con l'avvento di linguaggi come C e, più tardi, Python, è stata introdotta la standardizzazione attraverso librerie dedicate.

Alternative? Python offre anche il modulo `time`, ma `datetime` è più ricco di funzionalità e di solito preferito. Dettagli di implementazione: `datetime.now()` può accettare un fuso orario; senza, restituisce l'ora locale. Inoltre, `datetime` ha metodi per formattare la data (`strftime`) e analizzarla da stringa (`strptime`).

## Vedere Anche
- Documentazione ufficiale di `datetime`: https://docs.python.org/3/library/datetime.html
- PyMOTW sull'uso di `datetime` in Python: https://pymotw.com/3/datetime/
- Tutorial W3Schools sulla gestione della data e l'ora in Python: https://www.w3schools.com/python/python_datetime.asp
