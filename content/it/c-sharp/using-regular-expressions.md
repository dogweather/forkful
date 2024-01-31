---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Le espressioni regolari (regex) filtrano e manipolano il testo. Vengono utilizzate per la loro potenza ed efficienza nell'eseguire matching e sostituzioni complesse in stringhe.

## Come Fare:
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string testo = "Salve, il mio numero è: 123-45-6789.";
        string pattern = @"\d{3}-\d{2}-\d{4}";

        Match match = Regex.Match(testo, pattern);
        
        if(match.Success)
        {
            Console.WriteLine($"Numero trovato: {match.Value}");
        }
    }
}
```
Output:
```
Numero trovato: 123-45-6789
```

## Approfondimento
Le espressioni regolari sono nate negli anni '50. Alternativa alle regex sono le classiche funzioni stringa, però meno potenti per compiti complessi di matching. Le regex in C# sono implementate nel namespace `System.Text.RegularExpressions` e sono basate sulle regex Perl.

## Vedi Anche
- [MSDN Documentation on Regular Expressions](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Regex Tester and Debugger](https://regex101.com/)
- [Regex Quick Reference](https://www.rexegg.com/regex-quickstart.html)
