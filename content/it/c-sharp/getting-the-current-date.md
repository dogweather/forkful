---
title:                "Ottenere la data corrente"
html_title:           "C#: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Se stai scrivendo un programma in C#, è probabile che tu voglia includere la data corrente all'interno del tuo codice. Questo può essere utile per una varietà di motivi, come tenere traccia del tempo passato dall'avvio del programma o per sincronizzare l'orario del computer con un server remoto.

## Come fare
Per ottenere la data corrente in C#, puoi utilizzare la classe `DateTime.Now`. Ecco un esempio di codice che stampa la data corrente nel formato GG/MM/AAAA:
```
DateTime now = DateTime.Now;
Console.WriteLine(now.ToString("dd/MM/yyyy"));
```
Questo è il risultato dell'esecuzione del codice sopra riportato:
> 18/04/2021

Inoltre, puoi personalizzare il formato della data aggiungendo ulteriori argomenti alla funzione `ToString()`. Ad esempio, se vuoi includere anche l'ora e i minuti, puoi utilizzare il seguente codice:
```
DateTime now = DateTime.Now;
Console.WriteLine(now.ToString("dd/MM/yyyy HH:mm"));
```
Che produrrà un risultato come questo:
> 18/04/2021 20:30

## Approfondimento
La classe `DateTime` offre molti altri metodi e proprietà utili per gestire le date e gli orari in C#. Ad esempio, puoi utilizzare il metodo `AddDays()` per aggiungere o sottrarre un numero specifico di giorni alla data corrente. Puoi anche utilizzare il metodo `ToShortDateString()` per ottenere solo la parte della data senza l'ora.

Inoltre, puoi confrontare due date utilizzando gli operatori di confronto come `>` (maggiore di) e `<` (minore di). Questo può essere utile per verificare se una data è precedente o successiva a un'altra.

## Vedi anche
- Documentazione ufficiale di Microsoft su `DateTime` (https://docs.microsoft.com/it-it/dotnet/api/system.datetime)
- Tutorial su come gestire le date e gli orari in C# (https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- Esempi di codice per lavorare con le date in C# (https://www.c-sharpcorner.com/article/date-and-time-in-C-Sharp-programming/)