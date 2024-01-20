---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:37:59.437397-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Trasformare una data da stringa a un formato utilizzabile in programmi è essenziale per gestire input come date di nascita o scadenze. I programmatori fanno questo per manipolare, confrontare e archiviare date in modo efficiente.

## How to:
Utilizziamo `datetime` per fare magie con le date in Python. Ecco come si trasforma una stringa in una data:

```Python
from datetime import datetime

date_string = "1 aprile 2023"
date_object = datetime.strptime(date_string, "%d %B %Y")

print(date_object)
```

Ecco l'output:

```
2023-04-01 00:00:00
```

Vuoi un formato più europeo? Usiamo `strftime`:

```Python
formatted_date = date_object.strftime("%d/%m/%Y")
print(formatted_date)
```

Output:

```
01/04/2023
```

## Deep Dive
Parsing significa letteralmente "analisi sintattica", il che rimanda agli anni '70 quando si cominciò ad analizzare il linguaggio di programmazione. In Python, `datetime.strptime` è lo standard per convertire stringhe in date. Il metodo analizza la stringa basandosi su formati come `%d` per il giorno e `%m` per il mese. Ci sono anche librerie alternative come `dateutil` che sono più permissive con i formati.

Dettagli di implementazione? `datetime.strptime` è costruito su funzioni C, rendendolo veloce. Se però hai a che fare con moltissime date, ci sono moduli come `ciso8601` che possono fare il lavoro ancora più in fretta.

## See Also
Vuoi scavare ancora più a fondo? Dai un'occhiata a questi:

- Documentazione di `datetime`: https://docs.python.org/3/library/datetime.html
- `dateutil`, una libreria esterna per il parsing di date: https://dateutil.readthedocs.io/
- `ciso8601`, una libreria C per parsing veloci: https://github.com/closeio/ciso8601