---
title:                "Ottenere la data corrente"
aliases:
- /it/python/getting-the-current-date/
date:                  2024-02-03T19:10:29.137653-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ottenere la data corrente"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Ottenere la data corrente in Python è un'operazione fondamentale per molte applicazioni, come la registrazione degli eventi (logging), l'analisi dei dati e la presa di decisioni basate sul tempo. Si tratta di recuperare la data corrente del sistema, che è cruciale per compiti che dipendono dal contesto temporale.

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
