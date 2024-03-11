---
date: 2024-01-26 01:07:45.839285-07:00
description: "Il logging nella programmazione \xE8 come tenere un diario per la tua\
  \ applicazione. \xC8 la registrazione sistematica di eventi, messaggi e punti dati\
  \ che ti\u2026"
lastmod: '2024-03-11T00:14:17.580278-06:00'
model: gpt-4-1106-preview
summary: "Il logging nella programmazione \xE8 come tenere un diario per la tua applicazione.\
  \ \xC8 la registrazione sistematica di eventi, messaggi e punti dati che ti\u2026"
title: "Registrazione delle Attivit\xE0 (Logging)"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il logging nella programmazione è come tenere un diario per la tua applicazione. È la registrazione sistematica di eventi, messaggi e punti dati che ti danno una visione di ciò che la tua applicazione sta facendo e di come si sta comportando. I programmatori fanno logging perché è cruciale per il debugging, il monitoraggio della salute dell'applicazione e l'ottenimento di indizi su potenziali problemi prima che si trasformino in problemi reali.

## Come fare:
Ruby include un modulo integrato per il logging, `Logger`, che è super facile da usare. Ecco un esempio rapido per iniziare:

```ruby
require 'logger'

# Crea un Logger che scrive su STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Esempio di messaggi di log
logger.info("Questo è un messaggio informativo")
logger.warn("Questo è un messaggio di avvertimento")
logger.error("Questo è un messaggio di errore")
```

Eseguendo lo script sopra verrà prodotto qualcosa di simile a questo:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Questo è un messaggio informativo
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Questo è un messaggio di avvertimento
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Questo è un messaggio di errore
```

Puoi configurare il formato del log e il livello per filtrare il rumore inutile, e puoi indirizzare i log verso output diversi, come un file o persino un servizio di logging esterno.

## Approfondimento
Il logging è come una tradizione antica nella programmazione. Storicamente, i log erano semplici file di testo, analizzati manualmente con strumenti come `grep`. Ma il concetto si è sviluppato in un intero ecosistema di robusti framework e servizi di logging come Log4j, Syslog su Linux o Sematext e Loggly nell'era del cloud.

Il `Logger` di Ruby è un modo semplice per iniziare, ma se hai bisogno di più potenza e flessibilità, potresti provare alternative come Lograge o Semantic Logger. Queste librerie si integrano bene con le applicazioni Ruby, offrendo un controllo più granulare sulla formattazione dei log, inclusi log strutturati (formato JSON), prestazioni migliori e integrazione senza soluzione di continuità con altri servizi.

Ogni libreria di logging di Ruby ha il suo modo di fare le cose, ma in fondo, tutte si basano sull'idea di un'istanza di logger a cui invii messaggi. Il logger gestisce questi messaggi in base a livelli impostati—DEBUG, INFO, WARN, ERROR, FATAL e UNKNOWN—e decide cosa farne: stamparli, salvarli su file, inviarli attraverso la rete, ecc.

## Vedi Anche
Per un approfondimento sul modulo di logging integrato di Ruby, consulta la documentazione ufficiale:

Se sei interessato a un logging più avanzato o vuoi esplorare gemme di terze parti:
- [Lograge](https://github.com/roidrage/lograge)

Per pratiche e filosofie generali di logging (non specifiche di Ruby), questi articoli sono letture senza tempo:
- [Libro di Google Site Reliability Engineering - Capitolo 16: Handling Overload](https://sre.google/sre-book/handling-overload/#log-messages)
- [L'App 12 Factor - Logs](https://12factor.net/logs)
