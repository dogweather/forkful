---
title:    "C#: Concatenazione di stringhe"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Perché
Concatenare stringhe è un'operazione comune nella programmazione, che consente di unire due o più stringhe in una sola. Questo può essere utile per creare output personalizzati o per costruire URL dinamici.

## Come fare
Per concatenare le stringhe in C#, è possibile utilizzare l'operatore "+" o il metodo `Concat()` della classe `String`. Di seguito è riportato un esempio di entrambi i modi, con un output di esempio:

```c#
// Utilizzo dell'operatore "+"
string saluto = "Ciao";
string nome = "Mario";
string messaggio = saluto + " " + nome; // messaggio diventa "Ciao Mario"
Console.WriteLine(messaggio);

// Utilizzo del metodo Concat()
string frase1 = "Benvenuto";
string frase2 = "al mio blog";
string fraseCombinata = String.Concat(frase1, " ", frase2); // fraseCombinata diventa "Benvenuto al mio blog"
Console.WriteLine(fraseCombinata);

/* Output:
Ciao Mario
Benvenuto al mio blog
*/
```

## Approfondimento
Ci sono alcune cose da tenere a mente quando si concatenano stringhe in C#. Prima di tutto, l'operatore "+" è comunemente utilizzato, ma è importante ricordare che viene effettuata una nuova allocazione di memoria ogni volta che viene utilizzato. Quindi, se si sta concatenando molte stringhe, potrebbe essere più efficiente utilizzare il metodo `Concat()`.

Inoltre, è possibile concatenare stringhe con altri tipi di dati, come numeri o booleani. In questo caso, è necessario convertire questi tipi di dati in stringhe utilizzando i metodi `ToString()` o `Convert.ToString()`.

Infine, è importante tenere traccia dei separatori tra le stringhe correttamente. È possibile utilizzare un semplice spazio vuoto, come nell'esempio precedente, oppure un carattere specifico come la virgola o il punto e virgola.

## Vedi anche
- [Microsoft Docs: Stringhe in C#](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/strings/)
- [Concatenazione di stringhe in C#](https://www.c-sharpcorner.com/blogs/concatenation-of-strings-using-concat-method-in-c-sharp-programming-learn-to-code-like-a-pro) (in italiano)
- [Tutorial: Imparare a concatenare stringhe in C#](https://emorroidaman.it/programmazione/tutorial-imparare-a-programmare-in-c-concatenare-stringe/) (in italiano)