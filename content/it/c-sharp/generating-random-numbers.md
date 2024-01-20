---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

---
#### ## Cos'è & Perché?

Generare numeri casuali in programmazione significa creare numeri che non seguono un modello prevedibile. Questo è essenziale in molte applicazioni, come nei giochi per generare eventi imprevedibili, in sicurezza informatica per creare password uniche e per l'implementazione di algoritmi di machine learning.

#### ## Come fare:

Ecco un esempio su come generare un numero casuale in C#:

```C#
// Namespace necessario per il Random
using System;

class Program
{
    static void Main()
    {
        // Instanziare un oggetto Random
        Random rand = new Random();

        // Generare un numero casuale tra 0 e 100
        int numeroCasuale = rand.Next(0, 100);

        // Stampare il numero
        Console.WriteLine("Numero casuale generato: " + numeroCasuale);
    }
}
```

In output vedremo alla fine un numero casuale tra 0 e 100.

#### ## Approfondimento

Random è una classe C# integrata che fornisce la funzionalità per generare numeri casuali. Dal punto di vista storico, la generazione di numeri casuali era complessa e comportava l'uso di algoritmi matematici particolari. In C#, tutto questo è nascosto dietro la semplice interfaccia della classe Random.

Ci sono alternative alla classe Random in C#, come la classe RNGCryptoServiceProvider per i casi in cui la sicurezza è più importante della velocità. Questa classe implementa un generatore di numeri casuali più sicuro che non è prevedibile, ma è più lento del Random tradizionale.

La classe Random genera un flusso di numeri casuali partendo da un punto di partenza, o "seed". Due istanze di Random con lo stesso seed genereranno lo stesso flusso di numeri casuali.

#### ## Vedi anche:

- Documentazione Microsoft su [Random](https://docs.microsoft.com/it-it/dotnet/api/system.random?view=net-5.0)
- Tutorial Microsoft su [generazione di numeri casuali](https://docs.microsoft.com/it-it/dotnet/csharp/tutorials/random-numbers) 
- Discussione StackOverflow sul [migliore modo per generare numeri casuali in C#](https://stackoverflow.com/questions/2706500/simplest-way-to-create-a-random-number)
---