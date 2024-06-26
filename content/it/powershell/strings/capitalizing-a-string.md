---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:04.947604-07:00
description: "Come fare: PowerShell, essendo uno strumento versatile, ti consente\
  \ di capitalizzare una stringa utilizzando metodi semplici senza la necessit\xE0\
  \ di\u2026"
lastmod: '2024-03-13T22:44:43.624800-06:00'
model: gpt-4-0125-preview
summary: "PowerShell, essendo uno strumento versatile, ti consente di capitalizzare\
  \ una stringa utilizzando metodi semplici senza la necessit\xE0 di librerie di terze\
  \ parti."
title: Capitalizzare una stringa
weight: 2
---

## Come fare:
PowerShell, essendo uno strumento versatile, ti consente di capitalizzare una stringa utilizzando metodi semplici senza la necessità di librerie di terze parti. Ecco come puoi farlo:

```powershell
# Utilizzando il metodo built-in .Net 'ToTitleCase' da CultureInfo
$text = "ciao mondo"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$capitalizedText = $culture.TextInfo.ToTitleCase($text.ToLower())
Write-Output $capitalizedText
```
Output:
```
Ciao mondo
```

Nota: Questo metodo capitalizza la prima lettera di ogni parola. Se vuoi espressamente capitalizzare solo la prima lettera della stringa e lasciare il resto come è, potresti fare qualcosa del genere:

```powershell
# Capitalizzando solo il primo carattere di una stringa
$text = "ciao mondo"
$capitalizedText = $text.Substring(0,1).ToUpper() + $text.Substring(1)
Write-Output $capitalizedText
```
Output:
```
Ciao mondo
```

PowerShell non include direttamente una funzione semplice per capitalizzare solo la prima lettera di una stringa, ma combinando i metodi di manipolazione delle stringhe di base come `Substring(0,1).ToUpper()` e la concatenazione, possiamo facilmente ottenere il risultato desiderato.
