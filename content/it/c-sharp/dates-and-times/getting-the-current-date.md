---
title:                "Ottenere la data corrente"
aliases:
- /it/c-sharp/getting-the-current-date.md
date:                  2024-02-03T19:09:13.729724-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ottenere la data corrente"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Ottenere la data corrente in C# implica il recupero dei dettagli della data e dell'ora correnti dal sistema. I programmatori spesso hanno bisogno di accedere a queste informazioni per eseguire operazioni di registrazione, timestamping o pianificazione delle attività all'interno delle applicazioni, assicurando che le azioni siano temporizzate con precisione e i dati siano contrassegnati con timestamp precisi.

## Come fare:
C# fornisce un modo semplice per ottenere la data corrente usando la classe `DateTime` che fa parte del namespace System del .NET Framework. L'esempio sottostante dimostra come ottenere la data corrente, e, opzionalmente, l'ora.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Ottiene solo la data corrente
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Output: MM/dd/yyyy
        
        // Ottiene la data e l'ora corrente
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Output: MM/dd/yyyy HH:mm:ss

        // Ottiene la data e l'ora corrente UTC
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Output: MM/dd/yyyy HH:mm:ss
    }
}
```

In termini di librerie di terze parti, NodaTime offre una robusta alternativa per la manipolazione di date e ora, includendo il recupero della data corrente in diversi calendari e fusi orari.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Utilizzando NodaTime per ottenere la data corrente nel calendario ISO
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Output: yyyy-MM-dd

        // Per date specifiche del fuso orario
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Output: yyyy-MM-dd
    }
}
```

Questo mostra l'utilizzo di base con la classe `DateTime` integrata e le capacità avanzate fornite da NodaTime, particolarmente utili per applicazioni che richiedono la gestione di diversi fusi orari o sistemi di calendario.
