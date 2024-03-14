---
date: 2024-01-26 00:59:27.816909-07:00
description: "Il logging \xE8 la pratica di registrare eventi, errori e altre informazioni\
  \ significative dai processi in esecuzione di un programma su un file o uno stream\u2026"
lastmod: '2024-03-13T22:44:43.605926-06:00'
model: gpt-4-1106-preview
summary: "Il logging \xE8 la pratica di registrare eventi, errori e altre informazioni\
  \ significative dai processi in esecuzione di un programma su un file o uno stream\u2026"
title: Registrazione Eventi (Logging)
---

{{< edit_this_page >}}

## Cosa e perché?

Il logging è la pratica di registrare eventi, errori e altre informazioni significative dai processi in esecuzione di un programma su un file o uno stream di output. I programmatori lo fanno per tracciare il comportamento delle loro applicazioni, correggere problemi e mantenere un registro storico delle operazioni che può aiutare nella risoluzione di problemi futuri.

## Come fare:

In Bash, il logging può essere semplice come reindirizzare o aggiungere l'output a un file. Ecco un esempio di base:

```Bash
echo "Avvio dello script..." >> script.log
# I comandi del tuo script qui
echo "Script completato il $(date)" >> script.log
```

Per qualcosa di più avanzato, potresti incorporare syslog per il logging a livello di sistema:

```Bash
logger "Messaggio personalizzato dal mio script"
```

Il comando `logger` invia un messaggio di log al servizio syslog, che poi lo gestisce in accordo con la configurazione syslog del sistema.

Esempio di output catturato in `script.log`:

```Bash
Avvio dello script...
Script completato il Mar 23 Mar 09:26:35 PDT 2021
```

## Approfondimento

Storicamente nei sistemi simili a Unix, il logging è stato facilitato dal servizio syslog, consentendo a diverse applicazioni e parti del sistema di registrare messaggi in modo centralizzato. Ciò consente l'implementazione di un meccanismo di logging standardizzato in tutto il sistema.

Quando si tratta di alternative, alcuni potrebbero considerare l'uso di `syslog-ng` o `rsyslog` per funzionalità di logging più avanzate, oppure scrivere log in un database di serie temporali per scopi analitici. Per applicazioni con livelli di complessità più elevati, l'uso di una libreria o applicazione di logging dedicata come Log4j (nell'ecosistema Java) o Monolog (in PHP), che possono fornire opzioni di logging strutturate e configurabili, potrebbe avere senso anche per un linguaggio di scripting come Bash.

Il modo in cui implementi il logging dipende fortemente dai requisiti della tua applicazione. Se hai bisogno solo di un output semplice per monitorare il progresso dello script, aggiungere messaggi a un file è facile e conveniente. Tuttavia, per un logging più scalabile e robusto, vorrai integrarti con un sistema di logging che supporti funzionalità come la rotazione dei log, i livelli di log e il logging remoto.

## Vedi anche

- Le pagine man per le funzioni `logger` e `syslog` sono sempre utili, prova `man logger` o `man syslog`.
- Per uno sguardo approfondito al logging di sistema, considera il leggere la documentazione di `rsyslog` e `syslog-ng`.
- Per scoprire di più sul contesto storico e i principi dietro il logging nei sistemi simili a Unix, il protocollo `Syslog` documentato nell'RFC 5424 fornisce informazioni comprensive.
