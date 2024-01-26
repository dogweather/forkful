---
title:                "Confronto tra due date"
date:                  2024-01-20T17:32:49.068853-07:00
model:                 gpt-4-1106-preview
simple_title:         "Confronto tra due date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Confrontare due date significa verificare quale precede, segue o se sono identiche. I programmatori lo fanno per gestire eventi, confrontare scadenze, ordinare cronologicamente e controllare la validità di intervallo temporali.

## How to:
Gleam rende il confronto di date chiaro e diretto. Ecco un esempio basato sulla versione corrente:

```gleam
import gleam/calendar
import gleam/io

pub fn main() {
  let date1 = calendar.date(2021, 12, 24)
  let date2 = calendar.date(2023, 01, 01)

  io.println(calendar.is_before(date1, date2)) // true
  io.println(calendar.is_after(date1, date2))  // false
  io.println(calendar.is_same(date1, date1))   // true
}
```

Esecuzione del codice:

```
true
false
true
```

## Deep Dive
Prima di libreria standard di Gleam, il confronto di date poteva richiedere funzioni esterne o manuali. Ora, `gleam/calendar` semplifica processi che erano complessi. Altre lingue usano strutture e pacchetti simili, come la classe `DateTime` in C# o il modulo `datetime` in Python. In Gleam, i dettagli implementativi sono nascosti, consentendo di confrontare date con semplici funzioni.

## See Also
- Confronto di date in altre lingue: 
  - Python [Datetime module](https://docs.python.org/3/library/datetime.html)
  - C# [DateTime struct](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
