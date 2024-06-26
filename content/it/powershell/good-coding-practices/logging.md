---
date: 2024-01-26 01:07:45.000812-07:00
description: 'Come fare: Ecco le informazioni di base per aggiungere un po'' di logging
  semplice nei tuoi script.'
lastmod: '2024-03-13T22:44:43.649146-06:00'
model: gpt-4-1106-preview
summary: Ecco le informazioni di base per aggiungere un po' di logging semplice nei
  tuoi script.
title: "Registrazione delle Attivit\xE0 (Logging)"
weight: 17
---

## Come fare:
Ecco le informazioni di base per aggiungere un po' di logging semplice nei tuoi script:

```PowerShell
# Creazione di un semplice messaggio di log
Write-Host "Info: Inizio del processo dello script."

# Scrittura su file
"Info: Questo è un messaggio di log." | Out-File -Append myLog.log

# Utilizzo del cmdlet integrato per un logging più dettagliato
Start-Transcript -Path "./detailedLog.log"
Write-Output "Attenzione: Qualcosa non quadra."
# ... il tuo script fa cose
Stop-Transcript

# Output di detailedLog.log
******************************
Trascrizione di Windows PowerShell inizio
Ora di inizio: 20230324112347
Username  : PShellGuru@example.com
Utente esecutore: PShellGuru@example.com
Nome configurazione: 
Macchina  : PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Applicazione host: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
ID processo: 2024
Versione PS: 7.1.2
```

Ora, nei tuoi log, c'è una descrizione dettagliata di ciò che il tuo codice ha fatto.

## Approfondimento:
Storicamente, il logging è vecchio quanto la programmazione stessa. È come il diario di bordo di un capitano, ma per il software. In passato, potevano essere stampate o macchine telescriventi; ora si tratta tutto di file e di sofisticati sistemi di gestione dei log.

Quando sei immerso nelle trincee di PowerShell, `Write-Host` è rapido e sporco, ma si limita a buttare testo nella console, non ideale per tenere registrazioni. `Out-File` ti fornisce un modo semplice per gettare testo in un file, ma per la vera sostanza, vorrai usare `Start-Transcript` e `Stop-Transcript` che registrano tutto – input, output, l'intero processo.

Alternative? Certo, se stai operando in un'impresa, potresti considerare il registro eventi di Windows o l'uso di software come Logstash, ma per gli script quotidiani, aderisci agli strumenti di PowerShell. Per quanto riguarda l'implementazione, ricorda di fare un logging intelligente – troppo poco ed è inutile, troppo e diventa solo rumore di fondo.

## Vedi anche:
Consulta questi per avere una panoramica completa sul logging in PowerShell:
