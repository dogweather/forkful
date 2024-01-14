---
title:    "C#: Ottenere la data corrente"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler ottenere la data corrente all'interno del tuo programma in C#. Forse vuoi registrarla in un file di log o utilizzarla per calcolare il tempo trascorso tra due eventi del programma. Indipendentemente dalla tua motivazione, è un'operazione molto comune e fondamentale in molte applicazioni.

## Come farlo
Per ottenere la data corrente in C#, puoi utilizzare la classe `DateTime`, che viene fornita dalla libreria standard di .NET. Vediamo un esempio di codice:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine("La data corrente è: " + currentDate.ToString("dd/MM/yyyy"));
```

In questo esempio, stiamo creando una nuova istanza della classe `DateTime` e assegnando ad essa il valore corrente fornito dal metodo `Now`. Successivamente, stiamo stampando la data corrente nel formato "giorno/mese/anno" utilizzando il metodo `ToString()`.

Il risultato dell'esecuzione del codice dovrebbe essere simile a questo:
```
La data corrente è: 17/06/2021
```

## Approfondimento
La classe `DateTime` offre molti altri metodi utili per lavorare con date e orari. Ad esempio, è possibile utilizzare il metodo `AddDays()` per aggiungere o sottrarre una determinata quantità di giorni alla data corrente. Inoltre, è possibile utilizzare il metodo `ToString()` con diversi formati per ottenere la stringa della data desiderata.

Per ulteriori informazioni su come utilizzare la classe `DateTime` e le sue funzionalità, ti consiglio di consultare la documentazione ufficiale di Microsoft (link in fondo all'articolo).

## Vedi anche
- [Documentazione ufficiale di Microsoft su DateTime](https://docs.microsoft.com/it-it/dotnet/api/system.datetime?view=net-5.0)
- [Tutorial su date e orari in C#](https://www.c-sharpcorner.com/article/dates-and-time-functions-in-c-sharp/)
- [Esempi di formattazione di date in C#](https://www.programiz.com/csharp-programming/datetime-format)