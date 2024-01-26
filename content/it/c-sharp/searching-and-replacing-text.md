---
title:                "Ricerca e sostituzione del testo"
date:                  2024-01-20T17:57:39.956262-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Che Cos'è e Perché?)
Cercare e sostituire testo è un'operazione che ti permette di trovare stringhe specifiche in un flusso di testo e cambiarle con altre. Programmatori la usano per refactoring del codice, pulizia dei dati, e per automatizzare la correzione di errori.

## How to (Come Fare)
Ecco un esempio semplice in C# per cercare e sostituire testo:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string originalText = "Buongiorno, il tempo è sereno a Roma oggi.";
        string pattern = "Roma";
        string replacement = "Milano";

        string newText = Regex.Replace(originalText, pattern, replacement);

        Console.WriteLine(newText);
    }
}
```

Output:
```
Buongiorno, il tempo è sereno a Milano oggi.
```

## Deep Dive (Approfondimento)
La ricerca e sostituzione di testo non è nulla di nuovo. È un concetto che esiste da quando le persone hanno iniziato a elaborare testi con i computer, pensa a comandi Unix come `sed`. In C#, `System.Text.RegularExpressions.Regex` è il tuo migliore alleato per questo lavoro. Alternativamente, per sostituzioni semplici, puoi usare `String.Replace`.

Dettagli di implementazione: `Regex.Replace` è potente perché può utilizzare espressioni regolari, rendendo possibile sostituire pattern di testo complessi e non semplici corrispondenze di stringhe.

## See Also (Vedi Anche)
- Microsoft Docs sulla classe [Regex](https://docs.microsoft.com/it-it/dotnet/api/system.text.regularexpressions.regex)
- Tutorial su [espressioni regolari in C#](https://docs.microsoft.com/it-it/dotnet/standard/base-types/regular-expressions)
- `sed` man page per comandi Unix [sed](https://www.gnu.org/software/sed/manual/sed.html)
