---
title:                "Cercare e sostituire testo"
html_title:           "C#: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La ricerca e la sostituzione di testo è un'attività comune per i programmatori. Consiste nel cercare una determinata stringa di testo all'interno di un documento e sostituirla con un'altra stringa. Questo è spesso fatto per risparmiare tempo e automatizzare il processo di modifica di un documento o di un codice.

## Come fare:
Ecco un esempio di codice in C# per cercare e sostituire una stringa all'interno di un documento:

```c#
string documento = "Questo è un esempio di ricerca e sostituzione di testo.";
documento = documento.Replace("esempio", "cassetta degli attrezzi");
Console.WriteLine(documento);
```

L'output è: "Questo è un cassetta degli attrezzi di ricerca e sostituzione di testo."

## Approfondimento:
La ricerca e la sostituzione di testo è una funzionalità comune in molti editor di testo e IDE (Integrated Development Environment). Può essere utilizzata anche nei linguaggi di programmazione come Python, Java e JavaScript. Di solito viene utilizzata per trovare e correggere errori all'interno del codice o per svolgere attività ripetitive.

In alternativa, è possibile utilizzare espressioni regolari per la ricerca e la sostituzione di testo. Questo è particolarmente utile quando si desidera cercare e sostituire stringhe di testo in modo più complesso, come ad esempio tutti i numeri all'interno di un documento. Questo metodo richiede una conoscenza più approfondita delle espressioni regolari e del loro utilizzo all'interno di un linguaggio di programmazione specifico.

## Vedi anche:
Per saperne di più sulla ricerca e la sostituzione di testo in C#:
- [Documentazione Microsoft su Replace method](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)
- [Tutorial su espressioni regolari in C#](https://www.c-sharpcorner.com/article/c-sharp-regular-expressions/) 
- [Video tutorial su ricerca e sostituzione di testo in Visual Studio](https://www.youtube.com/watch?v=9mPuH22kcHU)