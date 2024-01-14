---
title:                "C#: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Perché

Trovare la lunghezza di una stringa è un'operazione comune nella programmazione e può essere utile in molte situazioni diverse. Ad esempio, potresti voler verificare che una stringa abbia una lunghezza minima o massima specifica, o magari dovresti manipolare una stringa in modo da garantirne la lunghezza.

##Come fare

Per trovare la lunghezza di una stringa in C#, puoi utilizzare il metodo `.Length`, come mostrato nell'esempio di codice qui sotto:

```C#
string testString = "Questo è un testo!";
Console.WriteLine(testString.Length);
```

L'output sarà "18", poiché la stringa contiene 18 caratteri, inclusi gli spazi.

Puoi anche utilizzare il metodo `.Count()` per trovare il numero di elementi all'interno di una stringa, come ad esempio i caratteri "e" o "o":

```C#
string testString = "Questo è un testo!";
Console.WriteLine(testString.Count(x => x == 'e'));
```

In questo caso, l'output sarà "2", poiché ci sono due "e" nella stringa.

##Approfondimento

Per trovare la lunghezza di una stringa in C#, il framework deve scorrere tutti i caratteri della stringa e contare ogni elemento. Questo può sembrare un'operazione semplice, ma in realtà richiede tempo, soprattutto se la stringa è molto lunga.

Inoltre, la lunghezza di una stringa in C# è limitata a 2 gigabyte. Se una stringa supera questa dimensione, il metodo `.Length` restituirà un numero negativo, poiché non è in grado di gestire una lunghezza così grande.

##Vedi anche

- [Documentazione Microsoft su come trovare la lunghezza di una stringa in C#](https://docs.microsoft.com/it-it/dotnet/api/system.string.length?view=netframework-4.8)
- [Tutorial YouTube su come utilizzare il metodo `.Length` in C#](https://www.youtube.com/watch?v=8XsG_lnPnbQ)