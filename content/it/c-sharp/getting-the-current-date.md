---
title:                "C#: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Perché dovresti ottenere la data corrente nel tuo programma? Beh, ci sono molte ragioni per cui potresti averne bisogno. Ad esempio, potresti aver bisogno di salvare la data in cui è stato eseguito un'azione specifica, o potresti voler mostrare la data aggiornata nelle tue applicazioni.

## Come fare
Ci sono diversi modi per ottenere la data corrente in C#, ma il metodo più comune e semplice è utilizzare il metodo `DateTime.Now`. Vediamo un esempio di come utilizzarlo:

```C#
DateTime dataCorrente = DateTime.Now;
Console.WriteLine(dataCorrente);
```

Ecco un esempio di output che potresti ottenere:

```
3/25/2021 1:41:39 PM
```

Se vuoi ottenere la data in un formato specifico, puoi utilizzare il metodo `ToString()` e specificare il formato desiderato come parametro:

```C#
DateTime dataCorrente = DateTime.Now;
string dataFormattata = dataCorrente.ToString("dd/MM/yyyy");
Console.WriteLine(dataFormattata);
```

Ecco un altro esempio di output:

```
25/03/2021
```

## Approfondimento
Oltre al metodo `DateTime.Now`, ci sono altri modi per ottenere la data corrente in C#. Ad esempio, puoi utilizzare il metodo `DateTime.Today` per ottenere la data corrente senza l'ora e i minuti, o puoi utilizzare il metodo `DateTime.UtcNow` per ottenere la data e l'ora in formato UTC.

Inoltre, puoi configurare il formato di data e ora di default per la tua applicazione utilizzando la classe `CultureInfo` e il metodo `DateTimeFormat.Setting`. Questo è utile se vuoi che il formato di data e ora sia coerente in tutta l'applicazione.

## Vedi anche
- [Documentazione ufficiale di Microsoft su DateTime.Now](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-5.0)
- [Tutorial su come utilizzare le date e le ore in C#](https://www.c-sharpcorner.com/UploadFile/09bc96/dates-and-times-in-C-Sharp-programming/)
- [Guida su come utilizzare la classe CultureInfo in C#](https://www.c-sharpcorner.com/UploadFile/mahesh/how-to-convert-date-time-value-format-using-C-Sharp/)