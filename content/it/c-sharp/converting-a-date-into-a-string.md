---
title:                "Convertire una data in una stringa"
html_title:           "C#: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Convertire una data in una stringa è il processo di convertire una data, espressa come valore numerico, in una rappresentazione testuale. I programmatori spesso eseguono questa operazione per rendere più leggibile una data o per poterla manipolare e utilizzare in diverse parti del codice.

## Come:

```C#
DateTime dataOdierna = DateTime.Now;
string dataStringa = dataOdierna.ToString("dd/MM/yyyy");
Console.WriteLine(dataStringa); // Output: "26/06/2021"
```

Il codice sopra mostra come utilizzare il metodo `ToString()` della classe `DateTime` per convertire un oggetto data in una stringa. Possiamo specificare il formato della stringa utilizzando il parametro opzionale della funzione, in questo caso "dd/MM/yyyy" per ottenere una rappresentazione come "giorno/mese/anno".

## Approfondimenti:

La conversione di una data in una stringa ha una storia radicata nella necessità di rappresentare i tempi in modo più chiaro e standardizzato. Esistono anche altre alternative per rappresentare una data, come ad esempio l'utilizzo di int per rappresentare una data come il numero di giorni passati dal 1 gennaio 0001. Inoltre, la formattazione della stringa può essere personalizzata utilizzando uno specifico formato di data e ora.

## Vedi anche:

- [Documentazione ufficiale di Microsoft su `DateTime.ToString Method`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)
- [Tutorial di C# su la conversione di date in stringhe](https://www.w3schools.com/cs/cs_date_tostring.asp)