---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

---

## Che Cosa & Perché?
Leggere un file di testo è l'atto di aprire e recupero del relativo contenuto con un programma. I programmatori fanno questo per recuperare dati, parametri o input che possono essere contenuti in un file di testo.

## Come fare:
Per leggere un file di testo in PowerShell, usa il comando `Get-Content`. Ecco un esempio semplice:

```PowerShell
$content = Get-Content C:\path\to\yourfile.txt
```

In questo esempio, PowerShell legge il contenuto del file `yourfile.txt` e lo assegna alla variabile `$content`.

Questo è il risultato dell'esecuzione del comando:

```PowerShell
Hello, World!
These are the contents of your text file.
```

Si può leggere un file di testo linea per linea utilizzando un ciclo `ForEach`:

```PowerShell
Get-Content C:\path\to\yourfile.txt | ForEach-Object {
    Write-Host $_
}
```

Se il file contiene tre righe di testo, l'output sarà simile a questo:

```PowerShell
Line 1
Line 2
Line 3
```

## Approfondimento:
PowerShell, rilasciato per la prima volta nel 2006, ha sostituito il Prompt dei comandi come principale linguaggio di scripting di Windows. Leggere un file di testo è una delle sue funzionalità più fondamentali.

Un'alternativa al comando `Get-Content` è `Cat`, un alias ereditato dalla sintassi Unix. Tuttavia, `Get-Content` è preferibile perché è più autoesplicativo e rispetta la convenzione dei verbi di PowerShell.

Nota che `Get-Content` legge i file di testo in modo lineare. Cioè, non legge tutto il file in una volta, ma una linea alla volta. Questo lo rende molto efficiente per la lettura di file di grandi dimensioni. Inoltre, grazie alla pipeline di PowerShell, i dati possono essere elaborati mentre sono ancora in lettura.

## Vedi Anche:
Per approfondire, consulta questi collegamenti:
- [Get-Content](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
- [ForEach-Object](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.core/foreach-object?view=powershell-7.1)

---