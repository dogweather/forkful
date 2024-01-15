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

## Perché
Ci sono molte ragioni per cui potresti voler convertire una data in una stringa, tra cui la visualizzazione su un'interfaccia utente o l'archiviazione dei dati in un database.

## Come
La conversione di una data in una stringa in C# è un'operazione semplice, ma può essere fatta in diversi modi a seconda delle tue esigenze.

Nell'esempio seguente, useremo il metodo `ToString()` per convertire una data nel formato desiderato. Possiamo specificare il formato utilizzando la stringa di formato `d` per il formato della data e `t` per il formato dell'ora. 

```C#
DateTime now = DateTime.Now;
string dateToString = now.ToString("d");
string timeToString = now.ToString("t");
Console.WriteLine("Data: " + dateToString);
Console.WriteLine("Ora: " + timeToString);
```

Output: 

```
Data: 23/04/2021
Ora: 19:30
```

Se vuoi specificare un formato personalizzato, puoi utilizzare la stringa di formato `DateTime` come parametro nel metodo `ToString()`. Ad esempio, se vogliamo visualizzare la data e l'ora in un formato diverso,

```C#
string nowToString = now.ToString("dd/M/yyyy HH:mm");
```

Output: 

```
23/4/2021 19:30
```

## Deep Dive
In C#, la conversione di una data in una stringa viene eseguita utilizzando il metodo `ToString()`, che accetta come parametro una stringa di formato `DateTime`. Esistono molti simboli di formato diversi che possono essere utilizzati per ottenere un risultato desiderato.

Ad esempio, la `d` rappresenta il formato della data breve (es. "dd/MM/yyyy"), mentre la `D` rappresenta il formato della data esteso (es. "dddd, dd MMMM yyyy"). Inoltre, la `t` rappresenta il formato dell'ora breve (es. "HH:mm") e la `T` rappresenta il formato dell'ora lungo (es. "HH:mm:ss").

È importante notare che la stringa di formato `DateTime` è case-sensitive, quindi lettere maiuscole e minuscole fanno differenza.

Oltre ai simboli predefiniti, è possibile utilizzare anche altri caratteri per formattare la stringa. Ad esempio, il carattere `:` può essere utilizzato per separare la data dall'ora e il carattere `/` può essere utilizzato per separare giorno, mese e anno.

La documentazione ufficiale di Microsoft offre una lista completa dei simboli e dei caratteri di formato disponibili, quindi assicurati di consultarla per ottenere il risultato desiderato.

## See Also
- [Official Microsoft documentation on date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [C# DateTime.ToString() method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)