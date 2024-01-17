---
title:                "Interpolazione di una stringa"
html_title:           "PowerShell: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
L'interpolare una stringa è un modo per inserire valore o variabili dinamiche all'interno di una stringa statica. Questo è spesso utile per generare messaggi personalizzati o per creare output dinamici in base a variabili. I programmatori utilizzano l'interpolazione delle stringhe per automatizzare il processo di creazione di stringhe e rendere il codice più efficiente.

## Come fare:
Il modo più semplice per interpolare una stringa in PowerShell è utilizzare l'operatore `-f` seguito da una stringa con dei placeholder in cui inserire i valori. Esempio:
```PowerShell
$nome = "John"
$cognome = "Smith"
$messaggio = "Benvenuto, {0} {1}!" -f $nome, $cognome
Write-Output $messaggio
```
Output:
```
Benvenuto, John Smith!
```
È anche possibile utilizzare la sintassi del doppio dollaro `$()` per interpolare una variabile all'interno di una stringa. Esempio:
```PowerShell
$eta = 30
$messaggio = "Hai {0} anni!" -f $eta
Write-Output $messaggio
```
Output:
```
Hai 30 anni!
```

## Approfondimento:
L'interpolarzione delle stringhe è stata introdotta in PowerShell 3.0 ed è stata ispirata dalla funzionalità simile in C# e VB.NET. Un'alternativa all'interpolazione delle stringhe è l'utilizzo dei comandi `Concat()` o `Format()` per unire e formattare le stringhe. L'operatore `-f` può essere utilizzato anche per formattare valori all'interno delle stringhe, come ad esempio limitare il numero di decimali dopo la virgola. Per ulteriori informazioni sull'utilizzo dell'interpolazione delle stringhe, si consiglia di consultare la documentazione ufficiale di PowerShell.

## Vedi anche:
- [Documentazione ufficiale di PowerShell](https://docs.microsoft.com/en-us/powershell/)
- [Esempi di interpolazione delle stringhe in PowerShell](https://www.pdq.com/blog/string-interpolation-powershell/)
- [Differenze tra l'interpolazione delle stringhe e i comandi Format/Concat](https://www.zumbido.com/2018/04/12/string-interpolation-in-powershell/)