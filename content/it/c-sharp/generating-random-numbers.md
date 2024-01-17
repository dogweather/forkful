---
title:                "Generare numeri casuali"
html_title:           "C#: Generare numeri casuali"
simple_title:         "Generare numeri casuali"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

Cosa è e perché:
Generare numeri casuali è un processo comune per i programmatori, che consiste nell'ottenere numeri casuali che possono essere utilizzati nei loro programmi. Questo è utile in una varietà di situazioni, come la generazione di password, il testing di algoritmi o la creazione di giochi.

Come fare:
Per generare numeri casuali in C#, è possibile utilizzare la classe `Random`, che viene fornita con il linguaggio. Questa classe ha diversi metodi che consentono di ottenere numeri casuali di diversi tipi, come interi, numeri a virgola mobile o booleani.

```csharp
// genera un numero casuale compreso tra 1 e 10
Random random = new Random();
int numeroCasuale = random.Next(1, 11);
Console.WriteLine($"Il numero casuale generato è: {numeroCasuale}");
```

```
// output: Il numero casuale generato è: 7
```

In questo esempio, abbiamo utilizzato il metodo `Next()` della classe `Random`, che accetta due parametri: il primo indica il valore minimo del numero casuale da generare, mentre il secondo indica il valore massimo. Nota che il valore massimo è escluso, quindi per ottenere un numero compreso tra 1 e 10, abbiamo utilizzato 11 come valore massimo.

La classe `Random` ha altri metodi utili, come `NextDouble()`, che genera un numero casuale tra 0 e 1, e `NextBytes()`, che riempie un array di byte con numeri casuali.

Approfondimento:
Il concetto di generazione di numeri casuali ha origini antiche, ma è diventato più ampiamente utilizzato con lo sviluppo dei computer. Prima dell'avvento dei computer, i numeri casuali erano spesso ottenuti utilizzando tecniche come il lancio di dadi o la scelta dei numeri da una tabella di numeri casuali pubblicata.

Ci sono anche altre tecniche per generare numeri casuali, come l'utilizzo dei movimenti del mouse o della temperatura del processore per ottenere numeri imprevedibili.

In C#, è anche possibile utilizzare la classe `RNGCryptoServiceProvider` per generare numeri casuali in modo sicuro e crittograficamente forte.

Vedi anche:
- Documentazione ufficiale di Microsoft sulla classe `Random` in C#: https://docs.microsoft.com/it-it/dotnet/api/system.random?view=net-5.0
- Un articolo su diversi metodi per generare numeri casuali in C#: https://www.c-sharpcorner.com/UploadFile/amrish_deep/Generating-Random-number-in-C-Sharp/