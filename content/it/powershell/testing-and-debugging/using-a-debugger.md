---
aliases:
- /it/powershell/using-a-debugger/
date: 2024-01-26 03:50:50.886434-07:00
description: "Usare un debugger significa impostare dei breakpoint, eseguire passo\
  \ dopo passo il proprio codice, osservare le variabili e ispezionare lo stato del\u2026"
lastmod: 2024-02-18 23:08:56.098735
model: gpt-4-0125-preview
summary: "Usare un debugger significa impostare dei breakpoint, eseguire passo dopo\
  \ passo il proprio codice, osservare le variabili e ispezionare lo stato del\u2026"
title: Utilizzo di un debugger
---

{{< edit_this_page >}}

## Cosa & Perché?
Usare un debugger significa impostare dei breakpoint, eseguire passo dopo passo il proprio codice, osservare le variabili e ispezionare lo stato del proprio programma mentre è in esecuzione. È un cambiamento radicale per i programmatori perché identifica i bug e ci aiuta a comprendere quello che il nostro codice sta realmente facendo.

## Come fare:
In PowerShell, è possibile eseguire il debug degli script utilizzando l'ambiente di scripting integrato di PowerShell (ISE) integrato o Visual Studio Code (VS Code) con l'estensione PowerShell. Ecco come utilizzare i breakpoint in entrambi:

### PowerShell ISE:
```PowerShell
# Imposta un breakpoint su una linea specifica
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# Esegui il tuo script normalmente
.\MyScript.ps1

# Quando lo script raggiunge il breakpoint, puoi ispezionare le variabili
$myVariable

# Continua l'esecuzione
Continue
```

### Visual Studio Code:
```PowerShell
# Apri il tuo script PowerShell in VS Code.
# Fai clic a sinistra del numero di linea per impostare un breakpoint.
# Inizia il debug premendo F5 o cliccando su 'Inizia Debugging'.

# VS Code interromperà l'esecuzione al tuo breakpoint.
# Usa il pannello di debug per osservare le variabili, ispezionare lo stack delle chiamate e controllare il flusso.
```

Il debugging in entrambi gli ambienti consente di entrare (F11), passare sopra (F10) e uscire (Shift+F11) mentre si esegue il debug.

## Approfondimento
Storicamente, il debugging in PowerShell era un po' macchinoso; richiedeva molte righe di `Write-Host` per mostrare lo stato delle variabili o il classico metodo di tentativi ed errori. Con l'avvento di PowerShell ISE, e più recentemente, di VS Code con le sue ricche funzionalità di debugging, il debugging in PowerShell è diventato quasi intuitivo come nei linguaggi di programmazione completi.

Le alternative agli strumenti di debugging nativi di PowerShell includono strumenti di terze parti come PowerGUI o l'uso di IDE robusti come Visual Studio con un plugin PowerShell.

Quando si implementa un debugger, considerare l'ambito dello script, specialmente quando si lavora con script dot-sourced o moduli. I breakpoint possono essere basati su condizioni, cambiamenti di variabili o su linee, consentendo un controllo preciso durante una sessione di debugging.

Inoltre, con il passaggio a PowerShell Core (PowerShell multipiattaforma), il debugging è in gran parte passato nelle mani di VS Code, che fornisce un'esperienza coerente attraverso diverse piattaforme.

## Vedi anche
Per ulteriori informazioni sul debugging in PowerShell:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
