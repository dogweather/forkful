---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:08.297284-07:00
description: 'Come fare: In C#, lavori con gli array associativi usando la classe
  `Dictionary<TKey, TValue>`. Ecco un esempio rapido per iniziare.'
lastmod: '2024-03-13T22:44:43.427149-06:00'
model: gpt-4-0125-preview
summary: In C#, lavori con gli array associativi usando la classe `Dictionary<TKey,
  TValue>`.
title: Utilizzo di array associativi
weight: 15
---

## Come fare:
In C#, lavori con gli array associativi usando la classe `Dictionary<TKey, TValue>`. Ecco un esempio rapido per iniziare:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Creazione di un dizionario
        Dictionary<string, int> cestoDiFrutta = new Dictionary<string, int>();

        // Aggiunta di coppie chiave-valore
        cestoDiFrutta.Add("Mele", 5);
        cestoDiFrutta.Add("Arance", 10);

        // Accesso a un valore usando la sua chiave
        Console.WriteLine("Mele: " + cestoDiFrutta["Mele"]);
        
        // Aggiornamento di un valore
        cestoDiFrutta["Mele"] = 7;
        Console.WriteLine("Mele aggiornate: " + cestoDiFrutta["Mele"]);
        
        // Rimozione di una coppia chiave-valore
        cestoDiFrutta.Remove("Arance");

        // Iterazione sul dizionario
        foreach (var coppia in cestoDiFrutta)
        {
            Console.WriteLine(coppia.Key + ": " + coppia.Value);
        }
    }
}
```
Output dell'esempio:
```
Mele: 5
Mele aggiornate: 7
Mele: 7
```

Questo esempio mostra la creazione di un dizionario, l'aggiunta, l'accesso, l'aggiornamento e la rimozione degli elementi, e l'iterazione su di esso.

## Approfondimento
Il concetto di array associativi risale al loro utilizzo nei linguaggi di scripting come Perl e PHP, dove offrono flessibilità nella gestione delle collezioni di dati. In C#, `Dictionary<TKey, TValue>` è l'implementazione de facto, introdotta nel .NET Framework 2.0. Memorizza i dati in una tabella hash, assicurando ricerche, aggiunte e cancellazioni efficienti.

Tuttavia, vale la pena notare che, sebbene i dizionari siano incredibilmente versatili, potrebbero non essere sempre la scelta migliore. Per mantenere collezioni ordinate, potresti esaminare `SortedDictionary<TKey, TValue>` o `SortedList<TKey, TValue>`, che offrono un ordine ordinato al costo di operazioni di inserimento e rimozione più lente. Per scenari che richiedono sicurezza nei thread, `ConcurrentDictionary<TKey, TValue>` aggiunge sovraccarico ma assicura l'accesso sicuro da più thread senza blocchi manuali.

In definitiva, la scelta di un'implementazione di array associativo in C# dipende dalle tue esigenze specifiche riguardo all'ordine, alla performance e alla sicurezza dei thread.
