---
title:                "Analisi di una data da una stringa"
aliases:
- /it/c-sharp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:39.680527-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisi di una data da una stringa"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e perché?
Effettuare il parsing di una data da una stringa in C# implica la conversione delle rappresentazioni testuali di date e orari in un oggetto `DateTime`. Questo è fondamentale per le applicazioni che hanno la necessità di manipolare, memorizzare o visualizzare date e orari in formati diversi, come app di pianificazione, processori di log o qualsiasi sistema che gestisca l'input delle date da utenti o fonti esterne.

## Come fare:

**Parsing Basic:**

I metodi `DateTime.Parse` e `DateTime.TryParse` sono le opzioni principali per convertire una stringa in un `DateTime`. Ecco un esempio rapido:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Parsing riuscito: {parsedDate}");
}
else
{
    Console.WriteLine("Parsing fallito.");
}
// Output: Parsing riuscito: 12/04/2023 00:00:00
```

**Specificando una Cultura:**

A volte, è necessario effettuare il parsing di una stringa di data che è in un formato specifico di una cultura. Questo può essere realizzato utilizzando la classe `CultureInfo`:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Output: 12/04/2023 00:00:00
```

**Parsing Esatto con un Formato Specifico:**

Per scenari in cui le date arrivano in un formato specifico che potrebbe non essere standard, `DateTime.ParseExact` è molto utile:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Output: 12/04/2023 00:00:00
```

**Usando NodaTime:**

Per un parsing di date e orari ancora più robusto, considera l'utilizzo della popolare libreria di terze parti NodaTime. Offre un'ampia gamma di capacità di gestione di date/orari:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Parsing fallito.");
}
```

NodaTime offre un ampio supporto per i fusi orari, concetti di periodi e durate, e molti diversi sistemi di calendario, rendendolo una scelta potente per la manipolazione complessa di date e orari nelle applicazioni .NET.
