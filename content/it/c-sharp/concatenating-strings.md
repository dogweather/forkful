---
title:                "C#: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è uno degli aspetti fondamentali della programmazione in C#. Questa tecnica permette di combinare diverse stringhe in un'unica stringa più grande, che può essere utilizzata in molteplici modi. Ad esempio, potrebbe essere utile per creare un messaggio personalizzato o per manipolare i dati inseriti dall'utente.

## Come

Per concatenare stringhe in C#, è possibile utilizzare l'operatore "+" o il metodo `Concat()`, entrambi presenti nella classe `String`. Vediamo alcuni esempi:

```C#
// Utilizzando l'operatore +
string firstName = "Marco";
string lastName = "Rossi";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName); // Output: Marco Rossi

// Utilizzando il metodo Concat()
string sentence = String.Concat("Questo è ", "un esempio ", "di concatenazione ");
Console.WriteLine(sentence); // Output: Questo è un esempio di concatenazione
```

In entrambi i casi, è possibile concatenare più di due stringhe in una sola operazione. È importante notare che l'operatore "+" può essere utilizzato anche per concatenare una stringa con un altro tipo di dato, come un numero o una variabile di un altro tipo, mentre il metodo `Concat()` richiede che tutti gli argomenti siano di tipo `String`.

## Analisi Approfondita

La concatenazione di stringhe è una delle operazioni più comuni e semplici nella programmazione. Tuttavia, è importante tenere presente che ogni volta che viene concatenata una nuova stringa, in realtà viene creato un nuovo oggetto di tipo `String`. Questo può essere un problema nelle situazioni in cui è necessario concatenare un grande numero di stringhe, poiché potrebbe causare un rallentamento delle prestazioni del programma.

Per evitare questo problema, si può utilizzare la classe `StringBuilder`, che alloca inizialmente una memoria sufficiente per contenere tutte le stringhe che verranno concatenate. In questo modo, le prestazioni sono notevolmente migliorate quando si concatenano molte stringhe. Ecco un esempio:

```C#
StringBuilder sb = new StringBuilder("Stringa iniziale ");
sb.Append("concatenata ")
  .Append("con ")
  .Append("StringBuilder.");
Console.WriteLine(sb.ToString()); // Output: Stringa iniziale concatenata con StringBuilder.
```

## Vedi anche

- [Documentazione ufficiale di Microsoft su String.Concat](https://docs.microsoft.com/it-it/dotnet/api/system.string.concat)
- [Documentazione ufficiale di Microsoft su StringBuilder](https://docs.microsoft.com/it-it/dotnet/api/system.text.stringbuilder)