---
date: 2024-01-26 01:17:10.313249-07:00
description: "Il refactoring \xE8 il processo di ristrutturazione del codice informatico\
  \ esistente senza cambiarne il comportamento esterno. I programmatori lo fanno per\u2026"
lastmod: '2024-03-13T22:44:43.442243-06:00'
model: gpt-4-0125-preview
summary: "Il refactoring \xE8 il processo di ristrutturazione del codice informatico\
  \ esistente senza cambiarne il comportamento esterno. I programmatori lo fanno per\u2026"
title: Rifattorizzazione
---

{{< edit_this_page >}}

## Cos'è & Perché?

Il refactoring è il processo di ristrutturazione del codice informatico esistente senza cambiarne il comportamento esterno. I programmatori lo fanno per pulire il codice, migliorare la leggibilità, ridurre la complessità e migliorare la manutenibilità.

## Come fare:

Rifattorizziamo un semplice metodo C# che calcola e stampa la somma di un array di numeri:

Prima del Refactoring:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("La somma è " + sum);
    }
}
```

Dopo il Refactoring:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"La somma è {CalculateSum()}");
    }
}

// Utilizzo:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Con il refactoring, abbiamo separato le responsabilità, reso la classe `Calculator` più flessibile consentendole di accettare qualsiasi array di numeri e sfruttato LINQ per rendere il calcolo della somma più conciso.

## Approfondimento

Il refactoring affonda le sue radici nella comunità di programmazione Smalltalk ed è stato reso popolare negli anni '90 dal libro di Martin Fowler "Refactoring: Improving the Design of Existing Code". Nel corso degli anni, è diventato una parte fondamentale delle metodologie agili e delle buone pratiche di programmazione.

Ci sono vari approcci al refactoring, come Red-Green-Refactor nello Sviluppo Guidato dai Test (TDD). Ciò assicura che il refactoring non introduca bug partendo da un test che fallisce, facendolo passare e poi pulendo il codice.

Quando si implementa il refactoring, è cruciale avere una suite di test completa per garantire che nessuna funzionalità venga interrotta durante il processo. Strumenti di refactoring automatico, come ReSharper per C#, possono anche aiutare in questo processo fornendo modi sicuri per cambiare le strutture del codice. Tuttavia, gli strumenti dovrebbero essere complementari a una profonda comprensione del codice e dei principi di programmazione.

## Vedi Anche

- L'opera fondamentale di Martin Fowler sul Refactoring: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- La guida di Microsoft sul Refactoring in Visual Studio: [Refactoring (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Uno sguardo dettagliato ai pattern di Refactoring con esempi: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
