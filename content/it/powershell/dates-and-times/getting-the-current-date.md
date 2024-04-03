---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:37.226709-07:00
description: "Come fare: PowerShell fornisce cmdlet semplici per ottenere data e ora.\
  \ Il cmdlet `Get-Date` \xE8 lo strumento primario per questo scopo. Pu\xF2 restituire\
  \ la\u2026"
lastmod: '2024-03-13T22:44:43.653191-06:00'
model: gpt-4-0125-preview
summary: PowerShell fornisce cmdlet semplici per ottenere data e ora.
title: Ottenere la data corrente
weight: 29
---

## Come fare:
PowerShell fornisce cmdlet semplici per ottenere data e ora. Il cmdlet `Get-Date` è lo strumento primario per questo scopo. Può restituire la data e l'ora complete, che puoi formattare o manipolare secondo le tue esigenze.

```powershell
# Ottenere la data e l'ora correnti
Get-Date
```

**Output di esempio:**

```
Martedì, 5 Settembre 2023 9:46:02 AM
```

È anche possibile formattare l'output per visualizzare solo le informazioni necessarie, come solo la data o solo l'ora.

```powershell
# Ottenere solo la data corrente in un formato specifico
Get-Date -Format "yyyy-MM-dd"
```

**Output di esempio:**

```
2023-09-05
```

```powershell
# Ottenere solo l'ora corrente
Get-Date -Format "HH:mm:ss"
```

**Output di esempio:**

```
09:46:02
```

### Utilizzando la Classe .NET
PowerShell consente l'accesso diretto alle classi .NET, offrendo un modo alternativo di lavorare con date e orari.

```powershell
# Usando la classe DateTime .NET per ottenere la data e l'ora correnti
[System.DateTime]::Now
```

**Output di esempio:**

```
Martedì, 5 Settembre 2023 9:46:02 AM
```

Per l'ora UTC:

```powershell
# Usando la classe DateTime .NET per ottenere la data e l'ora UTC correnti
[System.DateTime]::UtcNow
```

**Output di esempio:**

```
Martedì, 5 Settembre 2023 1:46:02 PM
```

Questi comandi e classi forniscono opzioni potenti e flessibili per lavorare con date e orari in PowerShell, essenziali per molti compiti di scripting e automazione.
