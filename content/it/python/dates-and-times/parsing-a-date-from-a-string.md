---
aliases:
- /it/python/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:01.716180-07:00
description: "L'analisi di una data da una stringa implica la conversione di informazioni\
  \ testuali su data e ora in un oggetto datetime o in un formato strutturato\u2026"
lastmod: 2024-02-18 23:08:55.530305
model: gpt-4-0125-preview
summary: "L'analisi di una data da una stringa implica la conversione di informazioni\
  \ testuali su data e ora in un oggetto datetime o in un formato strutturato\u2026"
title: Analisi di una data da una stringa
---

{{< edit_this_page >}}

## Cosa e perché?
L'analisi di una data da una stringa implica la conversione di informazioni testuali su data e ora in un oggetto datetime o in un formato strutturato equivalente. Questo viene comunemente eseguito per consentire operazioni di aritmetica, confronto e formattazione delle date in modo che siano indipendenti dalla lingua e dalla regione. I programmatori lo fanno per gestire e manipolare efficientemente i dati temporali estratti da log, input dell'utente o fonti esterne.

## Come fare:
La libreria standard di Python fornisce il modulo `datetime`, che include il metodo `strptime` per questo scopo. Il metodo richiede due argomenti: la stringa della data e una direttiva di formato che specifica il pattern della stringa in input.

```python
from datetime import datetime

# Esempio di stringa
date_string = "2023-04-01 14:30:00"
# Analisi della stringa in oggetto datetime
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# Output: 2023-04-01 14:30:00
```

Per un'analisi della data più sfumata, specialmente quando si hanno a che fare con formati o località multiple, la libreria di terze parti `dateutil` può essere estremamente utile. Fornisce un modulo parser che può analizzare date in quasi qualsiasi formato di stringa.

```python
from dateutil import parser

# Esempi di stringhe
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# Utilizzando il parser di dateutil
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# Output: 2023-04-01 14:30:00
print(parsed_date2)
# Output: 2023-04-01 14:30:00
```

`dateutil` è abile nel gestire la maggior parte dei formati di data senza stringhe di formato esplicite, rendendolo una scelta versatile per applicazioni che devono trattare rappresentazioni di date diverse.
