---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:06:50.576729-07:00
description: "L'interpolazione di stringhe in C# consente di creare una nuova stringa\
  \ includendo espressioni all'interno di un letterale stringa, facilitando cos\xEC\
  \ la\u2026"
lastmod: '2024-03-13T22:44:43.419793-06:00'
model: gpt-4-0125-preview
summary: "L'interpolazione di stringhe in C# consente di creare una nuova stringa\
  \ includendo espressioni all'interno di un letterale stringa, facilitando cos\xEC\
  \ la formattazione e la concatenazione di stringhe."
title: Interpolazione di una stringa
weight: 8
---

## Cos'è e Perché?
L'interpolazione di stringhe in C# consente di creare una nuova stringa includendo espressioni all'interno di un letterale stringa, facilitando così la formattazione e la concatenazione di stringhe. I programmatori utilizzano questa funzionalità per migliorare la leggibilità e la manutenibilità del codice, specialmente quando si ha a che fare con contenuti di stringhe dinamici.

## Come fare:
In C#, l'interpolazione di stringhe è denotata da un segno di dollaro (`$`) seguito da un letterale stringa. I nomi delle variabili o le espressioni sono racchiusi tra parentesi graffe (`{}`).

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Ciao, {name}! Hai {age} anni.";
Console.WriteLine(interpolatedString);
// Output: Ciao, Jane! Hai 28 anni.
```

In un esempio più complesso, puoi eseguire operazioni o chiamare metodi all'interno delle parentesi graffe:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Prezzo totale: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Output: Prezzo totale: €59.97
```
Il modificatore di formato `:C2` all'interno delle parentesi graffe formatta il numero come una valuta con due decimali.

Per scenari che richiedono una formattazione più avanzata o la localizzazione, potresti considerare l'uso del metodo `string.Format` o di librerie come Humanizer. Humanizer può manipolare e visualizzare stringhe, date, orari, intervalli di tempo, numeri e quantità in un formato più leggibile per gli esseri umani. Di seguito è riportato un esempio di utilizzo di Humanizer per una manipolazione complessa delle stringhe. Nota che Humanizer non fa parte della libreria standard di .NET e richiede l'installazione del pacchetto NuGet `Humanizer`.

Prima, installa Humanizer tramite NuGet:

```
Install-Package Humanizer
```

Quindi, puoi usarlo come segue:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"L'evento è stato {dayDifference} giorni fa.".Humanize();
Console.WriteLine(humanized);
// A seconda della configurazione e della cultura, un possibile output: L'evento è stato 5 giorni fa.
```

Questo esempio dimostra l'uso di base. Humanizer supporta una vasta gamma di funzionalità che possono essere applicate a stringhe, date, numeri e altro ancora, rendendo le tue applicazioni più accessibili e intuitive.
