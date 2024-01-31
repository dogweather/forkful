---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:13:42.447174-07:00
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente in C# è semplice. I programmatori lo fanno per registrare eventi, timestamp, per funzioni di logging o per gestire funzionalità legate al tempo.

## How to:
Ecco come ottenere la data odierna in C#:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate.ToString("dd/MM/yyyy HH:mm:ss"));
    }
}
```

Output:

```
05/04/2023 14:53:07
```

## Deep Dive
In C#, `DateTime.Now` è un metodo che recupera la data e l'ora correnti dal sistema. Risale alle origini del .NET Framework. Diverse culture hanno formati diversi, ma `ToString` può formattare la data in qualunque modo si desideri. Esistono alternative come `DateTime.UtcNow` per il tempo universale coordinato o `DateTime.Today` per la sola data, senza il tempo.

Le particolarità di `DateTime.Now` includono:

1. **Fuso Orario Locale**: `Now` riflette il fuso orario del sistema.
2. **Precisione**: La precisione è di circa 10-15 millisecondi.
3. **Performance**: Per operazioni di alta frequenza, `DateTime.UtcNow` è più veloce per via del minor carico di calcolo del fuso orario.

Durante le operazioni con la data e l’ora, è importante gestire:

- **Timezone**: Assicurati di gestire le conversioni tra fusi orari se la tua applicazione è distribuita globalmente.
- **Cambiamenti dell'ora legale**: `DateTimeOffset` può essere più affidabile per gestire le transizioni dell'ora legale.
- **Cultura**: Quando si formattano o si analizzano stringhe di date, bisogna considerare la cultura (locale) dell'utente.

## See Also
Approfondisci le basi su `DateTime` nella [documentazione Microsoft](https://docs.microsoft.com/it-it/dotnet/api/system.datetime?view=net-6.0).

Guarda differenze tra `DateTime.Now` e `DateTime.UtcNow` [qui](https://docs.microsoft.com/it-it/dotnet/api/system.datetime.utcnow?view=net-6.0).

Per questioni di localizzazione e cultura, vedi [Globalization and localization](https://docs.microsoft.com/it-it/dotnet/standard/globalization-localization/).
