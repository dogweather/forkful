---
title:                "Registrazione Eventi (Logging)"
aliases: - /it/fish-shell/logging.md
date:                  2024-01-26T01:04:06.386758-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione Eventi (Logging)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/logging.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Il logging, essenzialmente, consiste nell'annotare ciò che la tua applicazione sta facendo – potremmo dire un diario, ma per il codice. I programmatori lo fanno per tenere traccia dei dettagli, come cambiamenti di stato, eventi di sistema e fastidiosi bug, assicurandosi che nessun inconveniente passi inosservato.

## Come fare:
In Fish, effettuare il logging può essere semplice come reindirizzare gli output standard e gli stream di errore verso un file. Facendo un'entrata nel log per indicare l'inizio e la fine del nostro script.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script avviato" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Script terminato" >> my_app.log
end

log_start
# ... le operazioni del tuo script ...
log_end

cat my_app.log
```

Ecco cosa vedrai in `my_app.log`:

```
2023-04-01 10:35:47  - Script avviato
2023-04-01 10:36:02  - Script terminato
```

Per un logging avanzato, puoi utilizzare funzioni con parametri per il livello del log e messaggi:

```fish
function log_message --argument messaggio
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "Questo è un messaggio informativo."
log_message ERROR "Qualcosa è andato storto!"
```

Un esempio di output in `my_app.log` sarà:
```
2023-04-01 10:35:47 [INFO] Questo è un messaggio informativo.
2023-04-01 10:35:49 [ERROR] Qualcosa è andato storto!
```

## Approfondimento
Storicamente, nei shell script il logging veniva effettuato con una serie di istruzioni `echo`, e anche se questa è ancora un'opzione praticabile, implementare sistemi più complessi può essere una sfida. Fish non dispone di un meccanismo di logging incorporato come altre shell o linguaggi di programmazione, quindi spesso è necessario sviluppare una propria soluzione.

Alternative al comando `echo` incorporato in Fish per il logging includono strumenti Unix come `syslog` o `logger`, che si interfacciano con il demone del log di sistema, offrendo un approccio più integrato al logging di eventi su tutto il sistema.

La semplicità di Fish ti consente di creare funzioni per gestire la verbosità del logging, impostando diversi livelli che puoi attivare o disattivare. Alcune implementazioni possono persino includere il nome dello script, il numero di riga e il timestamp, che facilita la risalita attraverso i passi che hanno portato a un evento.

## Vedi Anche
- La documentazione di Fish Shell sulla scrittura di funzioni: https://fishshell.com/docs/current/#syntax-function
- Consigli Base per la Scrittura di Script Shell: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Guida al Protocollo Syslog: https://tools.ietf.org/html/rfc5424
