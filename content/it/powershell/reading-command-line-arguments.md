---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Leggere gli argomenti della riga di comando significa prelevare input specifici che l'utente inserisce quando esegue il tuo script PowerShell. Questo è utile per rendere lo script più flessibile ed efficace, adattandolo a diverse situazioni senza doverlo modificare. 

## Come Fare:

Supponiamo che tu abbia uno script PowerShell di nome `MyScript.ps1`. Puoi passargli argomenti come questo:

```PowerShell
.\MyScript.ps1 -Arg1 valore1 -Arg2 valore2
```

All'interno del tuo script, puoi accedere a questi argomenti così:

```PowerShell
param (
   [String]$Arg1,
   [String]$Arg2
)
Write-Output "Arg1 is $Arg1"
Write-Output "Arg2 is $Arg2"
```

L'output sarebbe:

```PowerShell
Arg1 is valore1
Arg2 is valore2
```

## Approfondimento:

Historicamente, l'accesso agli argomenti della riga di comando era un compito standard nella programmazione con linguaggi come C e Java. PowerShell ha semplificato il compito attraverso l'uso della funzione `param`.

Un'alternativa per leggere gli argomenti sarebbe l'uso dell'array automatico `$args`, ma il suo uso è sconsigliato perché offre meno flessibilità e sicurezza di tipo.

L'interpretazione degli argomenti nella riga di comando avviene da sinistra a destra. Gli argomenti possono essere interpretati come nome del parametro, valore del parametro o argomento posizionale a seconda della loro posizione e del formato.

## Vedi Anche:

- [Documento di Microsoft su come utilizzare gli argomenti della riga di comando in PowerShell](https://docs.microsoft.com/it-it/powershell/scripting/learn/deep-dives/everything-about-array?view=powershell-7.1)
- [Guida di Microsoft su come scrivere script in PowerShell](https://docs.microsoft.com/it-it/powershell/scripting/learn/ps101/04-writing-commands?view=powershell-7.1)
- [Post del blog sul passaggio di argomenti ai tuoi script PowerShell](https://devblogs.microsoft.com/scripting/passing-parameters-to-a-script-in-an-automated-fashion/)