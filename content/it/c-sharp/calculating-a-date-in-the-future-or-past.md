---
title:    "C#: Calcolo di una data nel futuro o nel passato"
keywords: ["C#"]
---

{{< edit_this_page >}}

#Perché
Calcolare una data nel futuro o nel passato può essere utile in molti contesti, come nel pianificare un evento o nella gestione delle scadenze.

##Come fare
Per calcolare una data in C#, è possibile utilizzare il metodo `Add` della classe `DateTime`. Ad esempio, se si vuole aggiungere 5 giorni ad una data, si può scrivere così:

```C#
DateTime data = new DateTime(2020, 10, 10);
DateTime dataFutura = data.Add(TimeSpan.FromDays(5));
Console.WriteLine(dataFutura);
```

Questo codice produrrà l'output `15/10/2020`.

Per sottrarre invece un certo periodo di tempo da una data, si può utilizzare il metodo `Subtract` in modo simile. Ad esempio, se si vuole sapere quale sarà la data tra un mese esatto, si può scrivere così:

```C#
DateTime data = new DateTime(2020, 10, 10);
DateTime dataNelFuturo = data.AddMonths(1);
Console.WriteLine(dataNelFuturo);
```

L'output sarà `10/11/2020`.

##Approfondimento
La classe `DateTime` in C# offre molte opzioni per calcolare una data nel futuro o nel passato. Si possono utilizzare i metodi `Add` e `Subtract` per aggiungere o sottrarre giorni, mesi, anni, ore, minuti, secondi e anche millisecondi. Inoltre, è possibile specificare un intervallo di tempo tramite la classe `TimeSpan`, come nel primo esempio.

Inoltre, è importante notare che C# segue il formato di data internazionale `yyyy/MM/dd`, quindi è importante assicurarsi di utilizzare questo formato per evitare possibili errori di calcolo.

#Vedi anche
- Documentazione ufficiale su `DateTime` e i suoi metodi: https://docs.microsoft.com/it-it/dotnet/api/system.datetime?view=netcore-3.1
- Esempi di calcolo di date future e passate in C#: https://www.tutorialsteacher.com/csharp/csharp-datetime