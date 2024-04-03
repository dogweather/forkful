---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:29.137653-07:00
description: "Come fare: **Utilizzando la libreria standard `datetime`:** Il modulo\
  \ `datetime` nella libreria standard di Python fornisce classi per manipolare date\
  \ e\u2026"
lastmod: '2024-03-13T22:44:43.011470-06:00'
model: gpt-4-0125-preview
summary: '**Utilizzando la libreria standard `datetime`:**


  Il modulo `datetime` nella libreria standard di Python fornisce classi per manipolare
  date e orari.'
title: Ottenere la data corrente
weight: 29
---

## Come fare:
**Utilizzando la libreria standard `datetime`:**

Il modulo `datetime` nella libreria standard di Python fornisce classi per manipolare date e orari. Per ottenere la data corrente, si può utilizzare il metodo `date.today()`.

```python
from datetime import date

oggi = date.today()
print(oggi)  # Output: AAAA-MM-GG (es., 2023-04-05)
```

**Formattazione dell'orario:**

Se hai bisogno della data corrente in un formato diverso, il metodo `strftime` ti consente di specificare una formattazione personalizzata della data:

```python
from datetime import date

oggi = date.today()
data_formattata = oggi.strftime('%B %d, %Y')  # Formato di esempio: "April 05, 2023"
print(data_formattata)
```

**Usando `pendulum` per maggiore flessibilità (una libreria di terze parti popolare):**

`Pendulum` è una libreria di terze parti che offre un approccio più intuitivo alla gestione di date e orari in Python. Estende le funzionalità standard di datetime e semplifica la gestione dei fusi orari, tra le altre caratteristiche.

Prima di tutto, assicurati di aver installato `pendulum` tramite pip:

```shell
pip install pendulum
```

Poi, per ottenere la data corrente:

```python
import pendulum

oggi = pendulum.now().date()
print(oggi)  # Output: AAAA-MM-GG (es., 2023-04-05)
```

Con `pendulum`, la formattazione è altrettanto semplice e simile all'approccio `strftime`:

```python
import pendulum

oggi = pendulum.now()
data_formattata = oggi.to_formatted_date_string()  # Formato predefinito: "Apr 5, 2023"
print(data_formattata)
```
