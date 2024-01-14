---
title:                "C#: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data in futuro o passato può essere utile in molte situazioni, ad esempio per pianificare eventi o scadenze, o per analizzare dati storici.

## Come Fare

Per calcolare una data in futuro o passato in C#, è necessario utilizzare la classe "DateTime" e i suoi metodi e proprietà. Ecco un esempio di codice che mostra come calcolare una data tra 3 mesi:

```C#
DateTime oggi = DateTime.Today; // Ottieni la data di oggi
DateTime dataFutura = oggi.AddMonths(3); // Aggiungi 3 mesi alla data di oggi
Console.WriteLine(dataFutura.ToString("dd/MM/yyyy")); // Stampa la data in un formato specificato
```

L'output di questo codice sarà "03/09/2021", poiché oggi è il 3 giugno 2021.

## Approfondimento

Esistono molti altri metodi e proprietà della classe "DateTime" che possono essere utilizzati per calcolare date in futuro o passato in base a diverse esigenze, ad esempio "AddDays" per aggiungere giorni, "AddYears" per aggiungere anni, "Subtract" per sottrarre una quantità di tempo specificata e molti altri. È anche possibile utilizzare i metodi "Parse" e "TryParse" per convertire una stringa in un oggetto "DateTime" e viceversa.

Inoltre, in C# sono disponibili anche altre classi utili per lavorare con date, come ad esempio "DateTimeOffset", che tiene conto anche del fuso orario, e "TimeSpan", che rappresenta una durata di tempo.

## Vedi Anche

- Documentazione ufficiale di Microsoft sulla classe "DateTime": https://docs.microsoft.com/it-it/dotnet/api/system.datetime?view=net-5.0
- Tutorial su come utilizzare la classe "DateTime" in C#: https://www.tutorialspoint.com/csharp/csharp_datetime.htm
- Esempi di utilizzo dei metodi della classe "DateTime": https://www.c-sharpcorner.com/UploadFile/mahesh/datetemp06162006114534AM/datetemp.aspx