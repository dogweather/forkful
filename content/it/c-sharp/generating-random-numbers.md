---
title:                "Generazione di numeri casuali"
html_title:           "C#: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un aspetto fondamentale della programmazione e può essere utile per varie ragioni, come ad esempio nel gioco d'azzardo, nell'algoritmo di ordinamento casuale o nella generazione di password sicure per i siti web.

## Come fare

Per generare numeri casuali in C#, è possibile utilizzare la classe `Random`. Di seguito un esempio di codice che genera 5 numeri casuali compresi tra 1 e 10 e li stampa a schermo:

```C#
Random rnd = new Random();
for (int i = 0; i < 5; i++)
{
    int num = rnd.Next(1, 11);
    Console.WriteLine(num);
}
```

L'output sarà del tipo: `7, 2, 10, 5, 9`.

In questo esempio, `rnd` è l'oggetto della classe `Random` e il metodo `Next()` viene utilizzato per generare il numero casuale. Si specifica poi il range di numeri desiderato dentro le parentesi tonde.

Per generare numeri casuali di altri tipi, come ad esempio numeri decimali o booleani, è possibile utilizzare gli appositi metodi della classe `Random` come `NextDouble()` o `NextBoolean()`.

## Approfondimenti

In realtà, generare numeri veramente casuali è un'impresa difficile. I computer sono in grado di generare solo numeri pseudocasuali, che seguono un algoritmo matematico. Questo significa che, utilizzando lo stesso seme (o valore iniziale) per la classe `Random`, si otterrà sempre lo stesso sequenza di numeri casuali. Per ovviare a questo problema, è possibile utilizzare un seme diverso ogni volta che si istanzia la classe `Random`, ad esempio utilizzando `DateTime.Now.Ticks`.

Un'altra cosa da tenere presente è che i numeri generati dalla classe `Random` si basano su una distribuzione uniforme, cioè ogni numero ha la stessa probabilità di essere generato. Se invece si vuole una distribuzione diversa, come ad esempio una gaussiana, è necessario utilizzare algoritmi più complessi.

## Vedi anche

* Documentazione ufficiale di Microsoft per la classe `Random`
* Altri esempi di generazione di numeri casuali in C#