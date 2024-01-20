---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Estrarre le sottostringhe è l'operazione di estrarre porzioni di stringa da una stringa più grande. I programmatori fanno questo per manipolare e analizzare dati di testo.

## Come fare:

Ecco alcuni esempi di come estrarre sottostringhe in C#:
```C#
string str = "Ciao, sono un programmatore C#";
// Ricorda che gli indici iniziano da 0!
string substring = str.Substring(6); // output: "sono un programmatore C#"

string anotherSubstring = str.Substring(6, 4); // output: "sono"
```
`Substring(int startIndex)` e `Substring(int startIndex, int length)` sono i metodi che usiamo per estrarre sottostringhe. `startIndex` è l'indice da cui inizia la sottostringa, mentre `length` è il numero di caratteri da estrarre.

## Approfondimenti

C# fornisce molteplici modi per lavorare con le stringhe. La funzione `Substring` ha le sue radici nei primi giorni del linguaggio. È un'implementazione diretta del concetto di "sottostringa" che proviene dalla teoria degli insiemi.

Esistono altre alternative a `Substring`, come `Split`, `Replace`, ecc., ma queste funzioni hanno scopi diversi e specifici.

La funzione `Substring` è implementata internamente attraverso l'uso di un array di caratteri. Non crea una nuova stringa, invece restituisce un riferimento all'array di caratteri della stringa originale. 
Ricorda, però, che le stringhe in C# sono immutabili, quindi non puoi modificare la sottostringa senza creare una nuova stringa.

## Vedi Anche:

Per ulteriori informazioni sulle stringhe in C#, consulta i seguenti link:

-[Documentazione Microsoft sull'oggetto String](https://docs.microsoft.com/it-it/dotnet/api/system.string?view=net-5.0)
-[Guida alla programmazione C# - lavorare con le stringhe nel .NET](https://docs.microsoft.com/it-it/dotnet/csharp/programming-guide/strings/)