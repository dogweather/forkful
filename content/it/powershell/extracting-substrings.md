---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
L'estrazione di sottostringhe è l'atto di selezionare e recuperare uno specifico set di caratteri all'interno di una stringa più grande. I programmatori la usano per manipolare e analizzare dati basati su testi - indispensabile in molti scenari di lavoro.

## Come si fa:
Ecco un rapido esempio su come estraiamo le sottostringhe in PowerShell:

```PowerShell
$testoCompleto = "Ciao, questo è un esempio di PowerShell"
$sottostringa = $testoCompleto.Substring(6,7)
Write-Output $sottostringa
```
Quando lo eseguisci, riceverai il seguente output:
```PowerShell
questo 
```
Hai semplicemente estratto e stampato la sottostringa!

## Approfondimento
Nel contesto storico, l'estrazione di sottostringhe è una funzione chiave del linguaggio di scripting fin dall'inizio di bash e perl. 

Per quanto riguarda le alternative, affrontiamo le espressioni regolari. Rappresentano uno strumento potente per analizzare ed estrarre dati da stringhe, anche se possono essere troppo complicati per compiti semplici.

Per quanto riguarda l'implementazione, la funzione `Substring` inizia a contare dall'indice 0. Quindi, `Substring(6,7)` inizia al 6° carattere e estrae i successivi 7 caratteri.

## Guarda anche
Per saperne di più sulla gestione delle stringhe in PowerShell, dai un'occhiata a questi link: 
- [Microsoft Official Documentation on Substrings](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
- [Understanding PowerShell Substring Method](https://www.tutorialsteacher.com/articles/understanding-powershell-substring-method)