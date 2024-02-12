---
title:                "Avvio di un nuovo progetto"
aliases: - /it/powershell/starting-a-new-project.md
date:                  2024-01-20T18:04:02.923861-07:00
model:                 gpt-4-1106-preview
simple_title:         "Avvio di un nuovo progetto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Iniziamo un nuovo progetto per creare qualcosa da zero e per risolvere problemi specifici. I programmatori lo fanno per innovare, lavorare su idee fresche, e per mettere in pratica ciò che hanno imparato.

## Come fare:
Aprire PowerShell e usare questi comandi per iniziare un nuovo progetto:

```PowerShell
# Crea una nuova cartella per il tuo progetto
New-Item -Path 'C:\I miei progetti\NuovoProgetto' -ItemType Directory

# Vai alla cartella del nuovo progetto
Set-Location -Path 'C:\I miei progetti\NuovoProgetto'

# Inizializza un repository Git (se usi il controllo della versione)
git init

# Crea un nuovo file, esempio uno script PowerShell
New-Item -Name 'ScriptPrincipale.ps1' -ItemType File
```

Output esempio:
```
Directory: C:\I miei progetti

Mode                LastWriteTime         Length Name
----                -------------         ------ ----
d-----        12/3/2023   4:20 PM                NuovoProgetto
Initialized empty Git repository in C:/I miei progetti/NuovoProgetto/.git/
```

## Approfondimenti:
Una volta, iniziare un nuovo progetto significava solo aprire un editor di testo e scrivere codice. Ora, è più complesso. Creiamo repository Git, strutture di cartelle, configuriamo ambienti di sviluppo, e molto altro. Esistono alternative come IDE che impostano automaticamente l'ambiente (es. Visual Studio, Eclipse), ma con PowerShell abbiamo un controllo granulare e possiamo automatizzare tutto il processo. Implementare questo in PowerShell significa scriptare passi che vengono fatti una volta e replicarli rapidamente in futuro.

## Vedi anche:
- Guida di PowerShell: https://docs.microsoft.com/it-it/powershell/
- Fondamenti di Git: https://git-scm.com/book/it/v2
