---
title:                "Cancellazione dei caratteri corrispondenti a un determinato schema"
html_title:           "C#: Cancellazione dei caratteri corrispondenti a un determinato schema"
simple_title:         "Cancellazione dei caratteri corrispondenti a un determinato schema"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Cancellare i caratteri che corrispondono a un certo modello è una tecnica comune fra i programmatori per eliminare determinati dati da una stringa o da un file di testo. Spesso viene usata per pulire i dati da caratteri indesiderati, come le virgolette o gli spazi vuoti, o per estrarre informazioni di interesse da dati non strutturati. Questa operazione è particolarmente utile quando si lavora con grandi quantità di dati.

## Come fare:
Per cancellare i caratteri che corrispondono a un certo modello in C#, puoi utilizzare il metodo ```Remove()``` della classe ```String```. Ad esempio, se volessimo eliminare tutte le virgolette da una stringa, potremmo usare il seguente codice:

```C#
string testo = "Questo è un esempio di \"testo\" con delle virgolette.";
string testoSenzaVirgolette = testo.Remove(testo.IndexOf("\""), 1);
```

Il risultato sarebbe ```Questo è un esempio di testo con delle virgolette.```.

Per eliminare più caratteri, puoi usare il metodo ```Replace()```, sempre della classe ```String```. Ad esempio, se volessimo eliminare sia le virgolette che gli spazi vuoti da una stringa, potremmo usare il seguente codice:

```C#
string testo = "Testo con \"virgolette\" e spazi vuoti.";
string testoPulito = testo.Replace("\"", "").Replace(" ", "");
```

Il risultato sarebbe ```Testoconvirgoletteespazivuoti.```.

## Approfondimento:
Cancellare i caratteri che corrispondono a un certo modello è un'operazione che trova spesso applicazione nella manipolazione di dati, ad esempio nel data cleaning o nell'analisi di dati non strutturati. In passato, per svolgere questa operazione si doveva scrivere codice ad hoc, mentre oggi grazie ai metodi ```Remove()``` e ```Replace()``` è possibile effettuarla in modo più efficiente. Alcune alternative alla cancellazione di caratteri sono l'uso di espressioni regolari o l'utilizzo di algoritmi di filtro.

## Vedi anche:
Per approfondire l'argomento, puoi consultare la documentazione ufficiale di Microsoft su [String.Remove()](https://docs.microsoft.com/it-it/dotnet/api/system.string.remove?view=netcore-3.1) e [String.Replace()](https://docs.microsoft.com/it-it/dotnet/api/system.string.replace?view=netcore-3.1). Altri metodi utili per la manipolazione di stringhe sono [String.Split()](https://docs.microsoft.com/it-it/dotnet/api/system.string.split?view=netcore-3.1) e [String.Trim()](https://docs.microsoft.com/it-it/dotnet/api/system.string.trim?view=netcore-3.1).