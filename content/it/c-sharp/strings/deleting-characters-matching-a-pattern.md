---
title:                "Eliminazione di caratteri che corrispondono a un pattern"
date:                  2024-01-20T17:41:52.334260-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminazione di caratteri che corrispondono a un pattern"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
In C# eliminare caratteri che corrispondono a un pattern significa usare algoritmi per rimuovere sequenze specifiche di caratteri da una stringa. I programmatori lo fanno per normalizzare dati, validare input o pulire testo da caratteri non desiderati.

## Come fare:
Ecco una stringa esempio e un pattern che vogliamo eliminare. Facciamo uso delle espressioni regolari (`Regex`):

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string testoOriginale = "Ciao, mondo! 123.";
        string pattern = @"[\d.-]"; // Rimuovo numeri, punti e trattini.
        
        string testoPulito = Regex.Replace(testoOriginale, pattern, "");
        
        Console.WriteLine(testoPulito); // Output: "Ciao, mondo! "
    }
}
```

## Approfondimento
Storicamente, il pattern matching e la sostituzione di stringhe sono stati semplificati con l'introduzione delle espressioni regolari (Regex), una feature che esiste da decenni in vari linguaggi di programmazione. In C#, `System.Text.RegularExpressions.Regex` fornisce potenti strumenti per queste operazioni. Alternative includono il metodo `String.Replace()` per sostituzioni semplici o l'uso di `StringBuilder` per modifiche più complesse e performanti. L'implementazione dipende dai requisiti: `Regex` è versatile ma può essere overkill per semplici sostituzioni.

## Vedi anche:
- [Documentazione C# Regex](https://docs.microsoft.com/it-it/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Tutorial su stringhe C#](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/strings/)
- [Espressioni regolari - MSDN](https://docs.microsoft.com/it-it/dotnet/standard/base-types/regular-expressions)
- [String.Replace Method](https://docs.microsoft.com/it-it/dotnet/api/system.string.replace?view=net-6.0)
