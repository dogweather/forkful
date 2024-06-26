---
date: 2024-01-20 17:42:53.591916-07:00
description: 'How to: Esempio 1: Rimuovere tutti i numeri da una stringa.'
lastmod: '2024-04-05T22:38:43.121750-06:00'
model: gpt-4-1106-preview
summary: 'Esempio 1: Rimuovere tutti i numeri da una stringa.'
title: Eliminazione di caratteri che corrispondono a un pattern
weight: 5
---

## How to:
Esempio 1: Rimuovere tutti i numeri da una stringa.

```PowerShell
$text = 'C4s4 Blanca 2023!'
$cleanText = $text -replace '[0-9]', ''
Write-Output $cleanText
```

Output:
```
C4s4 Blanca !
```

Esempio 2: Eliminare i caratteri speciali tranne gli spazi.

```PowerShell
$text = 'Benvenuti al Café@Sunrise!!'
$cleanText = $text -replace '[^\w\s]', ''
Write-Output $cleanText
```

Output:
```
Benvenuti al CaféSunrise
```

## Deep Dive
La cancellazione di caratteri seguendo un pattern è una funzionalità essenziale nei linguaggi di scripting come PowerShell, introdotta con le espressioni regolari (regex) nei primi linguaggi di programmazione. Usando `-replace`, PowerShell modernizza questa pratica antica. Un'alternativa in PowerShell è `[regex]::Replace()`, ma `-replace` è spesso più rapido per casi semplici. I dettagli di implementazione si affidano al motore regex .NET, che è potente e flessibile.

## See Also
- Dettagli sulle espressioni regolari in .NET: [Microsoft Docs - Regular Expressions .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- Come funzionano le espressioni regolari: [RegexOne](https://regexone.com/)
