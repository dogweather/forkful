---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Arduino: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

La lunghezza di una stringa rappresenta il numero di caratteri presenti in essa. I programmatori lo fanno spesso per avere un controllo preciso dei dati e gestire le operazioni sugli stessi in modo più efficace.

## Come si fa:

```C#
using System;

class Program {
    static void Main() {
        string testo = "Ciao a tutti!";
        Console.WriteLine(testo.Length);
    }
}
```
**Output:**
```
13
```
Qui, `.Length` ritorna il numero di caratteri nella stringa `testo`, che è "Ciao a tutti!" e stampa "13" sulla console.

## Approfondimenti:

1) **Contesto storico:** la proprietà `.Length` è presente in C# da quando il linguaggio è stato creato. È un modo semplice ed efficace di controllare la lunghezza di una stringa.

2) **Alternative:** ci sono varie tecniche per ottenere la lunghezza di una stringa, ma `.Length` è solitamente la più semplice e efficiente. Alcune alternative potrebbero includere l'iterazione sulla stringa e il conteggio dei caratteri, ma sono generalmente più lente.

3) **Dettagli implementativi:** in C#, quando si crea una stringa, l'oggetto memorizza la lunghezza della stringa per facilitare l'accesso rapido. Quindi, chiamando la proprietà `.Length`, non ci sarà alcun ritardo dovuto al conteggio dei caratteri.

## Vedi anche:

1) [Documentazione ufficiale .NET su String.Length](https://docs.microsoft.com/it-it/dotnet/api/system.string.length?view=net-5.0)
2) [Guida alla programmazione di stringhe in C#](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/strings/)
3) [Tutorial sulle operazioni di stringa in C#](https://www.c-sharpcorner.com/blogs/string-operations-in-c-sharp)