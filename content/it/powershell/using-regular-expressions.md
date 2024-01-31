---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"

category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Le espressioni regolari sono sequenze di caratteri che definiscono un pattern di ricerca. I programmatori le usano per cercare, sostituire o manipolare stringhe con precisione e velocità.

## How to:
```PowerShell
# Trovare una corrispondenza in una stringa
$testo = "Ciao, mondo! 123"
$pattern = "\b[^\d\W]+\b"
[regex]::Matches($testo, $pattern) | ForEach-Object { $_.Value }

# Sostituire le parole che iniziano con 'm' con 'X'
$testo = "mela casa mondo"
$pattern = "\bm\w*"
$resultato = [regex]::Replace($testo, $pattern, "X")
$resultato
```
Output:
```
Ciao
mondo
X casa X
```

## Deep Dive
Le espressioni regolari risalgono agli anni '50 con la loro teoria formalizzata da matematici come Stephen Kleene. In PowerShell, il modulo 'Microsoft.PowerShell.Utility' contiene cmdlet come `Select-String` che facilitano l'uso di regex. Come alternativa ai regex si possono usare metodi come `.Contains()`, `.StartsWith()`, `.EndsWith()` e `.IndexOf()` per ricerche più semplici.

## See Also
- Documentazione sulle espressioni regolari .NET: [docs.microsoft.com/dotnet/standard/base-types/regular-expressions](https://docs.microsoft.com/dotnet/standard/base-types/regular-expressions)
- Esercitazioni e test interattivi sui regex: [regex101.com](https://regex101.com)
