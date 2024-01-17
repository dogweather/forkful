---
title:                "Unione di stringhe"
html_title:           "C#: Unione di stringhe"
simple_title:         "Unione di stringhe"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

Cosa & Perché?

La concatenazione di stringhe è un'operazione comune nella programmazione in C#. Consiste nell'unire due o più stringhe per formarne una sola più lunga. I programmatori lo fanno spesso per creare messaggi di output complessi o per manipolare le stringhe in modo da ottenere il risultato desiderato.

Come fare:

```C#
// Definisci due stringhe
string primo = "Ciao";
string secondo = "mondo!";
// Concatenale utilizzando l'operatore +
string risultato = primo + " " + secondo;
// Output: "Ciao mondo!"
Console.WriteLine(risultato);
```
In questo esempio, abbiamo definito due stringhe, "Ciao" e "mondo!", e le abbiamo unite insieme utilizzando l'operatore + per creare una nuova stringa, "Ciao mondo!" Questo è solo un esempio semplice, ma la concatenazione di stringhe può essere utilizzata in modi più complessi, come ad esempio nel formato dei dati o nella generazione di output dinamici.

Profondità di immersione:

La concatenazione di stringhe è stata introdotta in C# 1.0 e da allora è diventata uno strumento fondamentale nella creazione di applicazioni. In passato, i programmatori utilizzavano l'operatore + per concatenare le stringhe, ma con l'introduzione di C# 6.0 è stato introdotto il nuovo operatore di interpolazione di stringhe, che semplifica la sintassi e rende più leggibile il codice.

In alternativa alla concatenazione di stringhe, i programmatori possono anche utilizzare la classe StringBuilder per unire le stringhe in modo più efficiente, specialmente se si lavora con grandi quantità di dati. Inoltre, è importante notare che la concatenazione di stringhe può essere una risorsa intensiva in termini di memoria, quindi è importante utilizzarla in modo oculato per evitare problemi di performance.

Per quanto riguarda l'implementazione della concatenazione di stringhe, il compilatore di C# traduce l'operatore + o l'operatore di interpolazione di stringhe in un metodo chiamato Concat() della classe String. Ciò significa che la concatenazione di stringhe è una parte integrante della classe String e può essere utilizzata con facilità in modo sicuro.

Vedi anche:

- [Documentazione Microsoft su Concatenazione di stringhe in C#](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/strings/)
- [Tutorial su come utilizzare l'operatore di interpolazione di stringhe in C#](https://www.tutorialspoint.com/csharp/csharp_string_interpolation.htm)
- [Documentazione Microsoft su StringBuilder in C#](https://docs.microsoft.com/it-it/dotnet/api/system.text.stringbuilder?view=net-5.0)