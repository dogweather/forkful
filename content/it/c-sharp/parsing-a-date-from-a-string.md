---
title:                "Estrarre una data da una stringa"
html_title:           "C#: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché:
Il parsing di una data da una stringa è il processo di estrapolare una data da una stringa di testo. I programmatori spesso lo fanno per convertire una data in un formato leggibile o per confrontare date in un'applicazione.

## Come:

```C#
using System;
public class Program
{
  public static void Main()
  {
     // Esempio di parsing di una data da una stringa in C#
     string stringaData = "04/09/2021";
     DateTime data = DateTime.ParseExact(stringaData, "dd/MM/yyyy", System.Globalization.CultureInfo.InvariantCulture);
    
     // Visualizzazione della data in un formato differente
     Console.WriteLine(data.ToString("MMMM d, yyyy"));
     // Output: Settembre 4, 2021
  }
}
```

## Approfondimento:

Il parsing di una data da una stringa è diventato una parte essenziale nella gestione delle date nelle applicazioni moderne. In passato, le date venivano spesso memorizzate come interi o long per risparmiare spazio di memoria. Con l'aumento delle capacità di archiviazione e la necessità di avere date in formati leggibili, il parsing dai testi è diventato molto comune.

Un'alternativa al parsing di una data da una stringa è l'utilizzo di librerie di terze parti come NodaTime o Moment.js. Queste librerie offrono funzionalità più avanzate per la gestione delle date e possono essere più adatte in determinati scenari.

Per implementare il parsing di una data da una stringa in C#, è possibile utilizzare il metodo `ParseExact` della classe `DateTime`. È importante specificare il formato della stringa di input utilizzando i simboli corretti come specificato dalla classe `CultureInfo`.

## Vedi anche:

- [Documentazione ufficiale su DateTime.ParseExact](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=net-5.0)
- [Libreria NodaTime](https://nodatime.org/)
- [Moment.js](https://momentjs.com/)