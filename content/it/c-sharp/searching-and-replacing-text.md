---
title:    "C#: Ricerca e sostituzione di testo"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
In programmazione, il sostituire testo è un'operazione comune che può aiutare a risparmiare tempo e rendere il codice più efficiente. Imparare come farlo correttamente può semplificare le modifiche di un grande volume di testo e migliorare la qualità del codice.

## Come fare
In C#, esistono diverse opzioni per cercare e sostituire del testo all'interno di una stringa. Una delle opzioni più semplici è utilizzare il metodo `Replace` della classe `String`. Ad esempio:

```C#
string frase = "Ciao a tutti!";
string nuovaFrase = frase.Replace("tutti", "amici");
Console.WriteLine(nuovaFrase);
```

Output: `Ciao a amici!`

È anche possibile utilizzare l'espressione regolare `Regex` per un modo più avanzato per cercare e sostituire del testo. Ad esempio:

```C#
string testo = "Questo è un testo di esempio.";
string nuovoTesto = Regex.Replace(testo, @"[aeiou]", "-");
Console.WriteLine(nuovoTesto);
```

Output: `Q--st- - un t-st- d- -mp--.`

## Approfondimento
Nel metodo `Replace` della classe `String`, è possibile specificare una terza opzione che indica quante sostituzioni devono essere effettuate. Questo è utile se si desidera sostituire solo alcune occorrenze di un determinato testo all'interno di una stringa.

```C#
string testo = "Questo è un testo di esempio.";
string nuovoTesto = testo.Replace("o", "-");
Console.WriteLine(nuovoTesto);
```

Output: `Quest- è un test- di emprpi-.`

Esistono anche altre opzioni avanzate quando si utilizza l'espressione regolare `Regex`, come la possibilità di specificare un limite di tempo per la ricerca e una combinazione di opzioni per una maggiore precisione nella sostituzione del testo.

## Vedi anche
- Documentazione Microsoft su come sostituire testo in una stringa in C#: https://docs.microsoft.com/it-it/dotnet/api/system.string.replace
- Tutorial su come utilizzare le espressioni regolari in C#: https://www.c-sharpcorner.com/article/c-sharp-regular-expressions-tutorial-with-examples/
- Domande frequenti su come effettuare ricerche e sostituzioni in C#: https://stackoverflow.com/questions/15534375/search-and-replace-in-c-sharp