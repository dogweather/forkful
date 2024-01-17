---
title:                "Maiuscolare una stringa"
html_title:           "C#: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cos'é & Perché?
Capitalize una stringa significa ottenere una nuova stringa in cui la prima lettera è maiuscola. I programmatori lo fanno per migliorare la leggibilità del testo o per rispettare determinati standard di codifica.

## Come:
```
C# 
string myString = "ciao mondo";
string capitalizedString = myString.Substring(0, 1).ToUpper() + myString.Substring(1);
Console.WriteLine(capitalizedString);
```
Output:
```
"Ciao mondo"
```

## Approfondimento:
Capitalize una stringa ha radici nei primi linguaggi di programmazione, dove la prima lettera di ogni parola doveva essere maiuscola per migliorare la comprensione dei comandi. Oggi, ci sono diverse alternative per capitalizzare una stringa come l'utilizzo di metodi specifici delle librerie di codifica o funzioni di manipolazione delle stringhe.

## Vedi anche:
- [Documentazione Microsoft su String.Substring()](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
- [Documentazione Microsoft su String.ToUpper()](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)
- [Altro modo per Capitalizzare una Stringa in C#](https://www.c-sharpcorner.com/article/capitalize-first-letter-of-a-string-in-c-sharp/)