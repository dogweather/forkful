---
title:    "C#: Convertire una data in una stringa."
keywords: ["C#"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore C# e ti trovi a dover lavorare con date in modo regolare, è importante saper convertire una data in una stringa. Questo può sembrare un compito semplice, ma può diventare complesso quando si considerano diversi formati di data e variabili di lingua.

## Come fare

Ci sono diverse soluzioni per convertire una data in una stringa in C#. Una delle opzioni più semplici è quella di utilizzare il metodo `ToString()` della classe `DateTime` come nell'esempio seguente:

```C#
DateTime data = new DateTime(2021, 5, 15);
string dataStringa = data.ToString();
Console.WriteLine(dataStringa); //Output: 5/15/2021 12:00:00 AM
```

In questo caso, la data viene convertita in una stringa nel formato predefinito del sistema operativo in cui viene eseguito il codice. Se si vuole specificare un formato di data diverso, è possibile passare un parametro di formato come nell'esempio seguente:

```C#
DateTime data = new DateTime(2021, 5, 15);
string dataStringa = data.ToString("d"); //Il parametro "d" indica il formato di data breve
Console.WriteLine(dataStringa); //Output: 5/15/2021
```

Ci sono molti altri parametri di formato disponibili per personalizzare la stringa di output della data. È anche possibile utilizzare il metodo `ToString()` su oggetti di tipo `DateTimeOffset` per convertire una data in una stringa insieme all'offset del fuso orario.

## Approfondimento

Per capire meglio il processo di conversione di una data in una stringa, è importante conoscere i concetti di formattazione della data e della localizzazione. Formattazione della data si riferisce alla trasformazione di una data in una stringa in base ad un determinato formato, mentre localizzazione fa riferimento alla possibilità di aggiungere varianti linguistiche e culturali alla stringa di output.

Per specificare un formato di data e una localizzazione personalizzati, è possibile utilizzare il metodo `ToString(string format, IFormatProvider provider)`. Il parametro `format` consente di specificare il formato di data desiderato e il parametro `provider` consente di specificare un'istanza di `IFormatProvider` che indica la localizzazione. Ad esempio:

```C#
DateTime data = new DateTime(2021, 5, 15);
CultureInfo provider = new CultureInfo("it-IT"); //Imposta la localizzazione su italiano
string dataStringa = data.ToString("ddd dd MMM yyyy", provider); //Il parametro "ddd" indica il giorno della settimana abbreviato e "MMM" indica il mese abbreviato
Console.WriteLine(dataStringa); //Output: sab 15 mag 2021
```

## Vedi anche

- [Documentazione ufficiale di Microsoft su ToString()](https://docs.microsoft.com/it-it/dotnet/api/system.datetime.tostring)
- [Formattazione della data e della localizzazione in C#](https://www.c-sharpcorner.com/article/working-with-datetimes-in-c-sharp-an-overview-of-parsing-formatting-and-times/)
- [Guida completa a CultureInfo in C#](https://docs.microsoft.com/it-it/dotnet/api/system.globalization.cultureinfo?view=net-5.0)