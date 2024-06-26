---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:01.716180-07:00
description: "Come fare: La libreria standard di Python fornisce il modulo `datetime`,\
  \ che include il metodo `strptime` per questo scopo. Il metodo richiede due\u2026"
lastmod: '2024-03-13T22:44:43.010434-06:00'
model: gpt-4-0125-preview
summary: La libreria standard di Python fornisce il modulo `datetime`, che include
  il metodo `strptime` per questo scopo.
title: Analisi di una data da una stringa
weight: 30
---

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
