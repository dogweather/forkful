---
title:    "C#: Generazione di numeri casuali"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui qualcuno potrebbe voler generare numeri casuali in un programma C#. Forse stai creando un programma di gioco che richiede elementi casuali, o forse vuoi aggiungere una funzionalità di sicurezza al tuo programma. In ogni caso, imparare come generare numeri casuali in C# può essere un'abilità utile da avere.

## Come
Ci sono vari modi per generare numeri casuali in C#. Uno dei modi più semplici è utilizzare la classe `Random` incorporata nella libreria standard di C#. Qui di seguito c'è un esempio di codice che genera un numero casuale compreso tra 1 e 10 e lo stampa in console:

```C#
Random rand = new Random();
int numeroCasuale = rand.Next(1, 11);
Console.WriteLine(numeroCasuale);
```

Questo codice crea un'istanza della classe `Random` e utilizza il metodo `Next` per generare un numero casuale tra 1 e 10. Puoi anche utilizzare il metodo `Next` per generare numeri casuali in un range diverso di valori.

```C#
int numeroCasuale = rand.Next(20, 51);
```

In questo esempio, il numero casuale sarà compreso tra 20 e 50 inclusi.

## Approfondimento
Se vuoi approfondire e imparare di più sui numeri casuali in C#, ci sono alcune cose importanti da tenere a mente. Innanzitutto, è importante sapere che i numeri generati dalla classe `Random` non sono veramente casuali, ma seguono un algoritmo predefinito. Quindi, se si digitano gli stessi parametri in un metodo `Next`, si otterrà sempre lo stesso numero.

Inoltre, la classe `Random` utilizza un valore chiamato "seed" per generare i numeri casuali. Questo valore può essere specificato in fase di creazione dell'istanza della classe o può essere generato automaticamente dal sistema operativo. Se vuoi ottenere numeri davvero casuali, puoi fornire un seed basato sul tempo di sistema utilizzando il metodo `Environment.TickCount`.

## Vedi Anche
- Documentazione ufficiale di Microsoft sulla classe `Random` in C# [https://docs.microsoft.com/it-it/dotnet/api/system.random](https://docs.microsoft.com/it-it/dotnet/api/system.random)
- Guida su come generare numeri casuali in C# [https://www.c-sharpcorner.com/article/generating-random-numbers-in-C-Sharp/](https://www.c-sharpcorner.com/article/generating-random-numbers-in-C-Sharp/)
- Esempi di codice per generare numeri casuali in C# [https://www.programiz.com/csharp-programming/examples/random-number](https://www.programiz.com/csharp-programming/examples/random-number)