---
title:                "C#: Confronto tra due date"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date può sembrare una semplice operazione nella programmazione, ma può essere molto utile per gestire e manipolare le date nel nostro codice. Questo articolo esplorerà come confrontare efficacemente due date utilizzando il linguaggio di programmazione C#. 

## Come fare

Per confrontare due date in C#, possiamo utilizzare il metodo `Compare()` della classe `DateTime`. Questo metodo accetta due parametri di tipo `DateTime` e restituisce un intero che indica la relazione tra le due date. 

```C#
DateTime data1 = new DateTime(2020, 10, 15);
DateTime data2 = new DateTime(2020, 10, 20);

int risultato = DateTime.Compare(data1, data2);
```

Nell'esempio sopra, il risultato sarà `-1`, poiché la data1 viene prima della data2 nella scala del tempo.

Possiamo anche utilizzare gli operatori di confronto `>, <, ==, !=` per comparare le date in modo più intuitivo.

```C#
DateTime data1 = new DateTime(2020, 10, 15);
DateTime data2 = new DateTime(2020, 10, 20);

if (data1 > data2)
{
    Console.WriteLine("La data1 è dopo la data2");
}
else if (data1 < data2)
{
    Console.WriteLine("La data1 è prima della data2");
}
```

L'esempio sopra stamperà `La data1 è prima della data2` poiché la data1 è antecedente alla data2 nella scala del tempo.

## Approfondimento

È importante notare che quando si confrontano date, si prendono in considerazione anche l'ora e il fuso orario. Se si desidera solo confrontare le date senza considerare queste informazioni aggiuntive, è possibile utilizzare il metodo `Date` per ottenere solo la parte della data da ciascuna data.

```C#
DateTime data1 = new DateTime(2020, 10, 15, 10, 30, 0);
DateTime data2 = new DateTime(2020, 10, 15, 15, 45, 0);

int risultato = DateTime.Compare(data1.Date, data2.Date); // risultato = 0
```

Inoltre, il metodo `Compare` restituisce i seguenti valori:

- Un valore negativo se il primo parametro è precedente al secondo parametro
- Un valore positivo se il primo parametro è successivo al secondo parametro
- 0 se i due parametri sono uguali

Infine, esiste anche il metodo `Equals()` che può essere utilizzato per confrontare due date e può essere utile se si vuole solo determinare se sono pari o meno.

## Vedi anche

- [Microsoft Docs - C# DateTime.Compare](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare)
- [Microsoft Docs - C# DateTime.Date](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.date)
- [Microsoft Docs - C# DateTime.Equals](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.equals)