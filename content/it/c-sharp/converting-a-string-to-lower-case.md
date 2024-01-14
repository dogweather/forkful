---
title:    "C#: Convertire una stringa in minuscolo"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una stringa in lettere minuscole è un'operazione comune in programmazione. Spesso è utile per uniformare il formato dei dati o per facilitare le operazioni di ricerca. In questo articolo vedremo come eseguire questa operazione utilizzando il linguaggio di programmazione C#.

## Come fare

In C#, esistono diversi modi per convertire una stringa in lettere minuscole. Uno dei più semplici è utilizzare il metodo `ToLower()` della classe `string`. Ecco un esempio di codice che mostra come farlo:

```
string myString = "CIAO A TUTTI";
string lowerString = myString.ToLower();
Console.WriteLine(lowerString);
```

L'output di questo codice sarà "ciao a tutti". Come puoi vedere, il metodo `ToLower()` ha convertito tutte le lettere della stringa in minuscolo.

Un'altra opzione per eseguire la conversione è utilizzare il metodo `ToUpperInvariant()`, che converte la stringa in lettere maiuscole utilizzando le regole invarianti della lingua inglese. Ad esempio:

```
string myString = "Ciao a tutti";
string upperString = myString.ToUpperInvariant();
Console.WriteLine(upperString);
```

L'output di questo codice sarà "CIAO A TUTTI". Questo metodo può essere utile se si vuole uniformare il formato delle stringhe senza dover gestire le differenze tra le lingue.

## Approfondisci

Oltre ai metodi `ToLower()` e `ToUpperInvariant()`, esistono altre tecniche per convertire una stringa in lettere minuscole in C#. Ad esempio, è possibile utilizzare il metodo `ToLower()` della classe `TextInfo`, che consente di gestire le differenze tra le lingue e i casi speciali come le lettere accentate. Inoltre, è possibile utilizzare espressioni regolari per convertire solo alcune parti della stringa in minuscolo.

In generale, è importante prestare attenzione alla lingua e alle regole di maiuscole/minuscole del testo che si sta elaborando per ottenere il risultato desiderato.

## Vedi anche

- [Metodo ToLower() della classe string (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [Metodo ToUpperInvariant() della classe string (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant)
- [Metodo ToLower() della classe TextInfo (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.tolower)
- [Espressioni regolari in C# (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)