---
title:                "Convertire una data in una stringa"
html_title:           "Python: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perchè

Ci sono molte ragioni per voler convertire una data in una stringa. Ad esempio, potresti dover stampare una data in un formato specifico per mostrare ai tuoi utenti o registrare la data in un database. Oppure, potresti voler calcolare la differenza tra due date in giorni, mesi o anni.

## Come fare

Per convertire una data in una stringa in Python, puoi utilizzare il metodo `strftime ()` della libreria standard `datetime`. Questo metodo accetta una stringa di formato come argomento e restituisce la data formattata in base al formato specificato.

Ecco un esempio di codice che converte la data corrente in una stringa nel formato "dd/mm/yyyy":

```python
from datetime import datetime

# ottieni la data corrente
today = datetime.now()

# usa il metodo strftime per convertire in una stringa
date_string = today.strftime('%d/%m/%Y')

# stampa la data formattata
print(date_string) # output: 19/04/2021
```

Puoi anche specificare il formato della data in base alle tue esigenze utilizzando diversi caratteri speciali. Ad esempio, `%d` rappresenta il giorno, `%m` rappresenta il mese e`%Y` rappresenta l'anno in quattro cifre.

Ecco un esempio di output per diverse stringhe di formato:

```python
# formato "gg/MM/YY"
date_string = today.strftime('%d/%m/%y')
# output: 19/04/21

# formato "Month Day, Year"
date_string = today.strftime('%B %d, %Y')
# output: April 19, 2021
```

## Approfondimento

Oltre al metodo `strftime()`, esistono anche altri modi per convertire una data in una stringa in Python. Ad esempio, puoi utilizzare la libreria di terze parti `arrow` che offre una sintassi più semplice e flessibile per la gestione delle date.

Inoltre, è importante tenere presente che le date possono essere soggette a fusi orari e che possono essere influenzate dalla configurazione locale del sistema. Perciò, se devi lavorare con più fusi orari, potresti voler considerare l'utilizzo della libreria `pytz` per gestire le date in modo più accurato.

## Vedi anche

- Documentazione ufficiale di Python sul metodo `strftime()`: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes
- Documentazione della libreria Arrow: https://arrow.readthedocs.io/en/latest/
- Documentazione della libreria Pytz: http://pytz.sourceforge.net/