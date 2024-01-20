---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Avviare un nuovo progetto significa creare di un nuovo set di script e file per un'applicazione o un servizio. I programmatori lo fanno per organizzare il codice, facilitare la collaborazione e gestire le versioni.

## Come fare:

Creare un nuovo progetto in PowerShell è piuttosto semplice. Abbiamo solo bisogno di creare una nuova cartella e poi creare nuovi script PowerShell all'interno di essa. Ecco un esempio:

```PowerShell
# Creiamo una nuova cartella chiamata 'MioProgetto'
New-Item -Path 'C:\' -Name 'MioProgetto' -ItemType 'directory'

# Andiamo nella nuova cartella che abbiamo appena creato
Set-Location -Path 'C:\MioProgetto'

# Creiamo un nuovo script PowerShell all'interno della nostra nuova cartella
New-Item -Name 'MioScript.ps1' -ItemType 'file'
```
Producendo il seguente output:

```PowerShell
Directory: C:\

Mode                LastWriteTime         Length Name                                                                          
----                -------------         ------ ----                                                                          
d-----       10/25/2021  10:12 AM                MioProgetto  

    Directory: C:\MioProgetto

Mode                LastWriteTime         Length Name                                                                          
----                -------------         ------ ----                                                                          
-a----       10/25/2021  10:12 AM              0 MioScript.ps1                              
```

## Approfondimento

Historicamente, i programmatori Windows utilizzavano batch script per automatizzare compiti. Tuttavia, PowerShell, rilasciato nel 2006, ha superato i batch script in termini di potenza e flessibilità. Offre un ambiente di scripting orientato agli oggetti e si integra strettamente con .NET Framework.

Malgrado questo, ci sono alcune alternative a PowerShell. Ad esempio, Python è popolare per i task di automazione ed è multi-piattaforma. Bash è un'opzione preferita per gli utenti Linux.

Attenzione, la creazione di un nuovo progetto tramite PowerShell è semplice, ma la gestione del progetto potrebbe non essere così semplice. Potremmo avere bisogno di un sistema di controllo delle versioni come Git e un server come GitHub. Per una gestione di progetto più complessa, considera un IDE come Visual Studio Code che ha supporto per PowerShell e molteplici estensioni utili.

## Guarda Anche

* [Guida di introduzione a PowerShell](https://docs.microsoft.com/it-it/powershell/scripting/overview?view=powershell-7.1)
* [Guida di Microsoft su come creare script PowerShell](https://docs.microsoft.com/it-it/powershell/scripting/learn/ps101/02-creating-scripts?view=powershell-7.1)
* [Syntaxis e strutture di comandi in PowerShell](https://docs.microsoft.com/it-it/powershell/scripting/learn/ps101/03-syntax-and-structures?view=powershell-7.1)
* [Visual Studio Code](https://code.visualstudio.com/) per la gestione avanzata dei progetti.