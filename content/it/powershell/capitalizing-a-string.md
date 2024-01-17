---
title:                "Maiuscolizzare una stringa"
html_title:           "PowerShell: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perché?

La capitalizzazione di una stringa significa rendere la prima lettera maiuscola e le restanti lettere minuscole. I programmatori lo fanno per rendere più leggibile il testo all'interno del codice e per uniformare lo stile del testo.

## Come fare:

```
PowerShell $stringa = "questa è una stringa di esempio" $stringa.ToUpper() ```
Output: "Questa è una stringa di esempio"

```
PowerShell $lista_nomi = "mario", "paola", "luca" $lista_nomi | ForEach-Object { $_.Substring(0,1).ToUpper() + $_.Substring(1) } ```
Output: "Mario", "Paola", "Luca"

## Deep Dive:

La capitalizzazione di una stringa è un'operazione comune utilizzata nella maggior parte dei linguaggi di programmazione. È utile per fornire maggiore leggibilità e uniformità al codice. Alcune alternative alla capitalizzazione sono l'utilizzo di metodi di formattazione del testo come "ToTitleCase" o l'uso di espressioni regolari per manipolare il testo.

Per implementare la capitalizzazione in PowerShell, è possibile utilizzare il metodo integrato "ToUpper()" o "ToTitleCase()" per le stringhe. Inoltre, è possibile utilizzare il comando "ForEach-Object" per eseguire la capitalizzazione di una lista di stringhe.

## Vedi anche:

- [Documentazione ufficiale di Microsoft su Capitalization](https://docs.microsoft.com/en-us/dotnet/standard/base-types/capitalization-conventions)
- [Tutorial su PowerShell](https://www.tutorialspoint.com/powershell/index.htm)