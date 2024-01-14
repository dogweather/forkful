---
title:                "C#: Calcolare una data nel futuro o nel passato"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Perché
Calcolare una data nel futuro o nel passato può essere un'operazione molto utile in diversi scenari di programmazione. Ad esempio, può essere utile per la gestione di promemoria o per la pianificazione di eventi futuri. Inoltre, può essere utilizzato per determinare il giorno della settimana di una data specifica o per calcolare il periodo di tempo tra due date.

# Come fare
Per calcolare una data in C#, è possibile utilizzare la classe "DateTime". Questa classe fornisce una serie di metodi utili per la gestione delle date, tra cui la possibilità di aggiungere o sottrarre giorni, mesi o anni a una data esistente.

Di seguito è riportato un esempio di codice che calcola la data di oggi e la data di oggi meno 7 giorni utilizzando la classe "DateTime":

```C#
DateTime oggi = DateTime.Today;
DateTime dataPassata = oggi.AddDays(-7);
Console.WriteLine("La data di oggi è: " + oggi);
Console.WriteLine("La data di oggi meno 7 giorni è: " + dataPassata);
```

L'output di questo codice sarebbe:

```
La data di oggi è: 09/04/2021 00:00:00
La data di oggi meno 7 giorni è: 02/04/2021 00:00:00
```

È anche possibile specificare una data specifica e utilizzare la classe "DateTime" per calcolare una data nel futuro o nel passato rispetto a quella specifica. Di seguito è riportato un esempio di codice che calcola la data di Pasqua di quest'anno:

```C#
DateTime dataPasqua = new DateTime(2021, 4, 4);
Console.WriteLine("La data di Pasqua di quest'anno è: " + dataPasqua);
```

L'output sarebbe:

```
La data di Pasqua di quest'anno è: 04/04/2021 00:00:00
```

# Approfondimento
Per un calcolo più preciso delle date, è possibile utilizzare la libreria "System.Globalization". Questa libreria fornisce metodi per gestire le differenze tra calendari, ad esempio il calendario gregoriano e il calendario giuliano. Inoltre, è possibile utilizzare la classe "DateTimeOffset" per gestire le date e gli orari in base al fuso orario.

Inoltre, è possibile utilizzare metodi della classe "DateTime" per ottenere informazioni più dettagliate sulla data, come il giorno della settimana o se è un anno bisestile.

# Vedi anche
- [Documentazione ufficiale di Microsoft su DateTime](https://docs.microsoft.com/it-it/dotnet/api/system.datetime)
- [Tutorial su DateTime e CultureInfo](https://www.c-sharpcorner.com/article/all-about-datetime-calculation-in-c-sharp-using-system-globalization/)
- [Guida su come utilizzare la classe System.Windows.Forms.DateTimePicker](https://www.dreamincode.net/forums/topic/277984-using-datetimepicker-in-c%23/)