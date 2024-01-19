---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
La ricerca e la sostituzione del testo sono operazioni frequente in programmazione che consistono nell'individuare specifiche stringhe di caratteri per modificarle o sostituirle. I programmatori fanno questo per manipolare i dati testuali, correggere errori, o aggiornare nomi di variabili o funzioni.

## Come Fare:
Ricercare e sostituire il testo in C# è molto semplice. Usiamo alcune funzioni di stringa, come `IndexOf`, `Substring`, e `Replace`. Ecco un esempio:

```C#
public class Program
{
    public static void Main()
    {
        string str = "Ciao Mondo!";
        string searchText = "Mondo";
        string replaceText = "Universo";

        int pos = str.IndexOf(searchText);

        if (pos != -1)
        {
            string result = str.Substring(0, pos) + replaceText + str.Substring(pos + searchText.Length);
            Console.WriteLine(result);  // Outputs: "Ciao Universo!"
        }
        // Or simply call Replace() function
        string replaceResult = str.Replace(searchText, replaceText);
        Console.WriteLine(replaceResult);  // Outputs: "Ciao Universo!"
    }
}
```
## Approfondimento
Historicamente, la ricerca del testo e la sostituzione sono concetti sviluppati dalle esigenze della programmazione. Con il crescere della complessità dei programmi, la necessità di trovare pattern specifici nel codice o nei dati è divenuta fondamentale.

Sebbene C# fornisca metodi comodi come `IndexOf`, `Substring` e `Replace`, esistono molte altre librerie e framework che offrono alternative più potenti e flessibili, come le espressioni regolari.

Nella sostituzione del testo, C# prima trova la corrispondenza esatta e poi inizia a sostituire dall'estremo sinistro. Inoltre, `Replace` sostituisce tutte le occorrenze. Se si vogliono sostituire solo le prime `n` occorrenze, una soluzione è svolgere un ciclo e usare `Substring` e `IndexOf` come nell'esempio sopra.

## Da Vedere Anche
- [Microsoft C# Guide - Strings](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [Microsoft C# Guide - Regular Expressions](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/regular-expressions/)
- [StackOverflow - Practical Use Cases of `String.Replace`](https://stackoverflow.com/questions/2289608/practical-use-cases-of-string-replace)