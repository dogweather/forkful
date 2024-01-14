---
title:    "C#: Estrazione di sottostringhe"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è un'operazione comune nella programmazione che consente di ottenere una parte specifica di una stringa più grande. Questa funzione può essere utile per la manipolazione di dati o per l'analisi di testi. In questo articolo, esploreremo come e perché utilizzare l'estrazione di sottostringhe in C#.

## Come fare

Per estrarre una sottostringa da una stringa in C#, utilizzeremo il metodo `Substring()` della classe `String`. Questo metodo accetta due parametri: l'indice iniziale della sottostringa e la lunghezza della sottostringa desiderata. Vediamo un esempio:

```C#
string frase = "Oggi è una bella giornata";
string sottostringa = frase.Substring(6, 3);
Console.WriteLine(sottostringa); // Output: è u
```

In questo esempio, abbiamo estratto una sottostringa di lunghezza 3 a partire dall'indice 6 della stringa `frase`, che corrisponde alle lettere "è u". Si noti che gli indici iniziano da 0 e che la lunghezza è opzionale - se non specificata, la sottostringa includerà tutti i caratteri fino alla fine della stringa.

Possiamo anche utilizzare il metodo `Substring()` per estrarre una parte di una stringa fino a un determinato carattere o parola. Vediamo un esempio:

```C#
string frase = "Benvenuti in Italia";
string sottostringa = frase.Substring(10);
Console.WriteLine(sottostringa); // Output: Italia
```

In questo caso, abbiamo specificato solo l'indice iniziale e non la lunghezza, quindi la sottostringa include tutti i caratteri dalla posizione 10 fino alla fine della stringa.

## Approfondimento

Oltre al metodo `Substring()`, esistono altri modi per estrarre sottostringhe in C#. Ad esempio, possiamo utilizzare il metodo `Split()` per dividere una stringa in più sottostringhe in base a un carattere o un insieme di caratteri specifici. Vediamo un esempio:

```C#
string numeri = "1, 2, 3, 4, 5";
string[] sottostringhe = numeri.Split(',');
Console.WriteLine(sottostringhe[2]); // Output: 3
```

In questo esempio, abbiamo diviso la stringa `numeri` ogni volta che incontriamo una virgola e abbiamo memorizzato le sottostringhe risultanti in un array. Possiamo quindi accedere alle singole sottostringhe utilizzando gli indici dell'array.

Un'altra opzione è utilizzare le espressioni regolari per estrarre sottostringhe in base a un modello. Questo è particolarmente utile quando si desidera estrarre parti specifiche di una stringa che seguono un determinato schema.

## Vedi anche

- [Documentazione di Microsoft su metodo `Substring()`](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [Tutorial su espressioni regolari in C#](https://www.c-sharpcorner.com/blogs/regular-expressions-in-c-sharp6)
- [Esempio di utilizzo del metodo `Split()`](https://www.tutorialsteacher.com/csharp/csharp-string-split)