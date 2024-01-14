---
title:                "C#: Estrazione di sottostringhe"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrazione di sottostringhe è un'importante funzionalità presente in molti linguaggi di programmazione, inclusa C#. Essa permette di ottenere solo una parte di una stringa più lunga, che può essere utile in diverse situazioni di programmazione.

## Come fare

Per estrarre sottostringhe in C#, è possibile utilizzare il metodo `Substring()` della classe `String`. Ecco un esempio di come potrebbe essere utilizzato:

```C#
string testo = "Questo è un testo di esempio.";

string sottostringa1 = testo.Substring(0, 6);
// sottostringa1 conterrà "Questo"

string sottostringa2 = testo.Substring(11, 5);
// sottostringa2 conterrà "testo"

string sottostringa3 = testo.Substring(17);
// sottostringa3 conterrà "un testo di esempio."
```

In questo esempio, il primo parametro del metodo `Substring()` indica l'indice di inizio della sottostringa, mentre il secondo parametro indica la lunghezza desiderata della sottostringa. Se viene fornito solo un parametro, verrà estratta la parte di stringa a partire da quell'indice fino alla fine.

L'output di questo esempio sarebbe il seguente:

```
sottostringa1: Questo
sottostringa2: testo
sottostringa3: un testo di esempio.
```

## Approfondimento

Oltre al metodo `Substring()`, C# offre anche altri strumenti per l'estrazione di sottostringhe, come ad esempio il metodo `Remove()` che permette di rimuovere una parte di una stringa. Inoltre, è possibile utilizzare espressioni regolari per trovare e estrarre particolari pattern di testo all'interno di una stringa.

L'estrazione di sottostringhe può essere utile in situazioni come la manipolazione di dati di testo, la creazione di password casuali, o la formattazione di output. È importante essere consapevoli delle funzionalità disponibili e di come utilizzarle correttamente per risolvere i problemi di programmazione che possono presentarsi.

## Vedi anche

- Documentazione Microsoft su `String.Substring()`: https://docs.microsoft.com/it-it/dotnet/api/system.string.substring
- Tutorial su espressioni regolari in C#: https://www.tutorialspoint.com/csharp/csharp_regular_expressions.htm
- Esempi di manipolazione di stringhe in C#: https://www.geeksforgeeks.org/c-sharp-string-class/