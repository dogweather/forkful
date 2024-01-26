---
title:                "Maiuscolizzare una stringa"
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e perché?)
Capitalizzare una stringa significa trasformare le lettere minuscole in maiuscole. I programmatori lo fanno per uniformità, leggibilità o per rispettare standard come i titoli o gli acronimi.

## How to: (Come fare:)
```C#
using System;

class CapitalizeStringExample
{
    static void Main()
    {
        string original = "ciao mondo!";
        string capitalized = original.ToUpper();
        
        Console.WriteLine("Originale: " + original);
        Console.WriteLine("Capitalizzata: " + capitalized);
    }
}
```
Output:
```
Originale: ciao mondo!
Capitalizzata: CIAO MONDO!
```

## Deep Dive (Approfondimento)
In C#, capitalizzare una stringa è diretto con ToUpper() o ToUpperInvariant(). ToUpper() tiene conto delle impostazioni locali (culturale), modificando la stringa rispetto alla lingua dell'utente. ToUpperInvariant usa invece i standard invarianti, spesso per dati tecnici.

Storicamente, la necessità di manipolare le lettere può risalire alle prime macchine da scrivere e computer che distinguevano tra maiuscole e minuscole. Oggi, abbiamo diversi metodi per manipolare le stringhe.

Alternative a ToUpper() includono:
- TextInfo.ToTitleCase(): per trasformare solamente la prima lettera di ogni parola in maiuscolo, tipicamente usato per i titoli dei libri o intestazioni.
- String.ToLower(): per trasformare in minuscolo, utile per uniformare le stringhe in input prima del confronto.

Dettagli di implementazione considerano la gestione di caratteri speciali e le implementazioni Unicode, che estendono i casi d'uso ben oltre il solo alfabeto inglese.

## See Also (Vedi Anche)
- Microsoft Docs su [String.ToUpper](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-6.0)
- Microsoft Docs su [String.ToUpperInvariant](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant?view=net-6.0)
- Microsoft Docs su [TextInfo.ToTitleCase](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=net-6.0)
- Introduzione a Unicode: [What is Unicode?](https://home.unicode.org/basic-info/overview/)
