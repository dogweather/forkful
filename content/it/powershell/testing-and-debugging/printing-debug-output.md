---
date: 2024-01-20 17:53:05.863059-07:00
description: "La stampa dei messaggi di debug \xE8 un modo per tracciare cosa sta\
  \ succedendo nel tuo script. I programmatori la usano per capire il comportamento\
  \ del\u2026"
lastmod: '2024-03-11T00:14:17.257642-06:00'
model: gpt-4-1106-preview
summary: "La stampa dei messaggi di debug \xE8 un modo per tracciare cosa sta succedendo\
  \ nel tuo script. I programmatori la usano per capire il comportamento del\u2026"
title: Stampa dell'output di debug
---

{{< edit_this_page >}}

## Che cosa e perché?
La stampa dei messaggi di debug è un modo per tracciare cosa sta succedendo nel tuo script. I programmatori la usano per capire il comportamento del codice e scovare gli errori.

## Come fare:
In PowerShell, usa `Write-Host`, `Write-Output`, o `Write-Debug` per mostrare i messaggi. Ecco degli esempi:

```PowerShell
# Usa Write-Host per stampare direttamente nella console
Write-Host "Questo è un messaggio di debug"

# Usa Write-Output per inviare l'output lungo la pipeline
Write-Output "Questo messaggio può essere passato ad altri cmdlet"

# Usa Write-Debug per inviare un messaggio di debug che può essere abilitato o disabilitato
Write-Debug "Questo è un messaggio di debug - sarà visibile solo se lo script è eseguito con il parametro -Debug"
```

Output:

```
Questo è un messaggio di debug
Questo messaggio può essere passato ad altri cmdlet
DEBUG: Questo è un messaggio di debug - sarà visibile solo se lo script è eseguito con il parametro -Debug
```

## Approfondimento:
Il debug in PowerShell si è evoluto con il linguaggio. Prima, `Write-Host` era l'opzione principale, ma i messaggi non potevano essere catturati o rediretti. `Write-Output` manda l'output lungo la pipeline, il che è più in linea con la filosofia PowerShell. `Write-Debug` è più specializzato per il debugging; i suoi messaggi sono nascosti a meno che non si usi il parametro `-Debug`.

Alternative includono `Write-Verbose` per messaggi informativi dettagliati e `Write-Warning` per avvisi non critici.

I dettagli dell'implementazione di queste cmdlet influenzano come gli script interagiscono con l'utente e altri cmdlet. Ad esempio, usare `Write-Host` può rendere il tuo script meno modulare, mentre `Write-Output` può essere la scelta migliore se hai bisogno che l'output sia utilizzato altrove.

## Link utili:
- Articolo su "When to use Write-Output vs. Write-Host": [Understanding Streams, Redirection, and Write-Host in PowerShell](https://devblogs.microsoft.com/scripting/understanding-streams-redirection-and-write-host-in-powershell/)
