---
title:                "Convertire una stringa in minuscolo"
html_title:           "C#: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Potresti essere interessato a convertire una stringa in minuscolo per facilitare la comparazione o la ricerca di dati, o semplicemente per una questione di preferenza personale.

## Come fare

Per convertire una stringa in minuscolo in C#, puoi utilizzare il metodo `ToLower()` della classe `System.String`. Ecco un esempio di codice e il relativo output:

```C#
string testString = "QUESTA STRINGA SARÀ CONVERTITA IN MINUSCOLO"; 
string convertedString = testString.ToLower(); 
Console.WriteLine(convertedString); 
```

Questo codice produrrà l'output:

```
questa stringa sarà convertita in minuscolo
```

## Approfondimento

La conversione di una stringa in minuscolo può sembrare un'operazione banale, ma è importante comprendere quali sono i dettagli e le possibili complicazioni che possono sorgere. Ad esempio, bisogna tenere presente che la conversione dipende dalla cultura e dalla lingua in cui si sta lavorando. Inoltre, è possibile specificare manualmente la cultura di riferimento nel metodo `ToLower()`, ad esempio utilizzando `ToLower(CultureInfo.CurrentCulture)` per utilizzare la cultura di sistema del computer su cui il codice viene eseguito.

## Vedi anche

- [Documentazione ufficiale Microsoft su stringhe in C#](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/strings/)
- [Guida su le culture e la localizzazione in .NET](https://docs.microsoft.com/it-it/dotnet/standard/globalization-localization/)