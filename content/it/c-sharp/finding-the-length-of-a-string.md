---
title:                "Calcolare la lunghezza di una stringa"
html_title:           "C#: Calcolare la lunghezza di una stringa"
simple_title:         "Calcolare la lunghezza di una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti aver bisogno di trovare la lunghezza di una stringa in un programma C#. Potresti voler mostrare al tuo utente quanti caratteri ha inserito, o forse hai bisogno di validare la lunghezza di una password. Indipendentemente dalla motivazione, sapere come trovare la lunghezza di una stringa è un'abilità fondamentale per ogni programmatore C#.

## Come fare

Per trovare la lunghezza di una stringa in C#, puoi utilizzare il metodo `Length` della classe `String`. Ecco un semplice esempio di come utilizzarlo:

```C#
string miaStringa = "Questo è un esempio.";
int lunghezza = miaStringa.Length;
Console.WriteLine(lunghezza); // Output: 21
```

In questo esempio, abbiamo creato una variabile `miaStringa` che contiene una stringa di testo. Poi abbiamo utilizzato il metodo `Length` per ottenere la lunghezza della stringa e l'abbiamo assegnata alla variabile `lunghezza`. Infine, abbiamo usato il metodo `WriteLine` della classe `Console` per stampare il valore della variabile `lunghezza`.

Puoi anche utilizzare il metodo `Length` direttamente all'interno di un'istruzione, senza dover assegnare il valore a una variabile. Ad esempio:

```C#
string miaStringa = "Esempio";
Console.WriteLine("La lunghezza della stringa è: " + miaStringa.Length); // Output: La lunghezza della stringa è: 7
```

In questo caso, abbiamo utilizzato il metodo `Length` all'interno di una stringa, concatenandolo con il testo desiderato che precede il valore della lunghezza.

## Approfondimenti

Il metodo `Length` è disponibile per qualsiasi oggetto di tipo `String` in C#. Tieni presente che questo valore rappresenta il numero di caratteri all'interno della stringa e non il numero di parole o spazi. Inoltre, la lunghezza di una stringa vuota sarà sempre uguale a 0.

Inoltre, è importante notare che il metodo `Length` considera tutti i caratteri all'interno della stringa, inclusi anche spazi e caratteri speciali come i segni di punteggiatura. Quindi, se vuoi escludere questi caratteri dall'essere conteggiati, dovrai utilizzare un approccio diverso per trovare la lunghezza della stringa.

## Vedi anche

- [Documentazione di Microsoft su come trovare la lunghezza di una stringa in C#](https://docs.microsoft.com/it-it/dotnet/api/system.string.length?view=net-5.0)
- [Esempi di utilizzo del metodo `Length`](https://www.tutorialspoint.com/csharp/csharp_string_length.htm)