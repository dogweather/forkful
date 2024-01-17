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

## Che cos'è e perché?

La conversione di una stringa in minuscolo è un'operazione comune tra i programmatori che permette di trasformare tutti i caratteri di una stringa in lettere minuscole. Questo rende semplice la comparazione di stringhe, in quanto le lettere maiuscole e minuscole vengono considerate uguali.

## Come fare:

Per convertire una stringa in minuscolo in C#, è necessario utilizzare il metodo ToLower(). Di seguito un esempio di codice che converte una stringa in minuscolo e ne stampa il risultato:

```C#
string s = "HELLO WORLD";
Console.WriteLine(s.ToLower());
```

Output:
```
hello world
```

## Approfondimento:

Questa operazione è stata implementata per la prima volta nel linguaggio di programmazione C e successivamente è stata incorporata in diversi altri linguaggi, incluso C#. Tuttavia, alcuni linguaggi hanno metodi alternativi come ToLowerInvariant(), che utilizza regole di conversione specifiche dell'implementazione per garantire la compatibilità tra piattaforme.

## Vedi anche:

- Documentazione ufficiale di Microsoft per il metodo ToLower() in C#: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower
- Approfondimenti sulla manipolazione di stringhe in C#: https://www.c-sharpcorner.com/UploadFile/84c85b/String-manipulation-with-C-Sharp/
- Spiegazione del concetto di case sensitivity (sensibilità alle maiuscole e minuscole): https://www.techopedia.com/definition/27628/case-sensitivity