---
date: 2024-01-26 01:08:22.551238-07:00
description: "Il logging \xE8 il processo di registrazione degli eventi di un'applicazione\
  \ mentre questa \xE8 in esecuzione, fornendo una traccia di briciole per analisi\u2026"
lastmod: '2024-03-11T00:14:16.562878-06:00'
model: gpt-4-1106-preview
summary: "Il logging \xE8 il processo di registrazione degli eventi di un'applicazione\
  \ mentre questa \xE8 in esecuzione, fornendo una traccia di briciole per analisi\u2026"
title: "Registrazione delle Attivit\xE0 (Logging)"
---

{{< edit_this_page >}}

## Cosa e Perché?
Il logging è il processo di registrazione degli eventi di un'applicazione mentre questa è in esecuzione, fornendo una traccia di briciole per analisi post-mortem e monitoraggio in tempo reale. I programmatori lo fanno perché aiuta a risolvere problemi, monitorare le prestazioni, e tracciare le azioni degli utenti per la sicurezza e fini analitici.

## Come fare:
Python include un modulo integrato per il logging. Ecco una configurazione di base:
```Python
import logging

# Configurazione di base del logging
logging.basicConfig(level=logging.INFO)

# Messaggi di logging
logging.debug('Questo è un messaggio di debug')
logging.info('Informazioni su quello che il tuo programma ha appena fatto')
logging.warning('Un messaggio di avvertimento')
logging.error('Si è verificato un errore')
logging.critical('Il programma non è in grado di riprendersi!')
```
Quando esegui questo codice, vedrai il seguente output (poiché il livello predefinito è WARNING, i messaggi di debug e info non verranno mostrati):
```
WARNING:root:Un messaggio di avvertimento
ERROR:root:Si è verificato un errore
CRITICAL:root:Il programma non è in grado di riprendersi!
```
Puoi anche configurare il logging per scrivere su un file anziché sulla console:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Ora i tuoi log verranno indirizzati al file 'app.log'.

## Approfondimento
Il logging è presente sin dagli albori della programmazione, con i log di sistema tra le forme più antiche di archiviazione persistente al di fuori dei file veri e propri che contengono dati. A parte la storia, il concetto principale del logging rimane sostanzialmente invariato, anche se gli strumenti si sono evoluti.

Il modulo `logging` di Python è piuttosto potente e flessibile. Consente ai programmatori di impostare diversi livelli di log (DEBUG, INFO, WARNING, ERROR, CRITICAL) che possono aiutare nella categorizzazione e nel filtraggio dei log. Ha un sistema di logger gerarchico, il che significa che è possibile avere relazioni di genitore-figlio tra i logger e propagare i messaggi lungo la catena.

Alternative includono librerie di terze parti come Loguru o structlog che offrono funzionalità migliorate e un'interfaccia più semplice rispetto al modulo di logging integrato. Possono fornire un output più gradevole, una migliore serializzazione dei dati strutturati e modi più intuitivi di gestire la configurazione dei log.

Per quanto riguarda l'implementazione, quando si configura il logging è importante farlo una sola volta all'inizio della tua applicazione. Configurarlo a livello di modulo è consigliato utilizzando `logging.getLogger(__name__)` per seguire le migliori pratiche di logging di Python.

Il logging non dovrebbe influenzare drasticamente le prestazioni di un'applicazione in circostanze normali. Tuttavia, bisogna stare attenti a ciò che viene registrato: un logging eccessivamente verboso, specialmente ai livelli DEBUG, può rallentare un'applicazione e riempire rapidamente lo spazio di archiviazione dei file di log.

## Vedi Anche
Per maggiori informazioni sul modulo di logging di Python, consulta il cookbook ufficiale di logging di Python per alcuni ottimi esempi e pratiche consigliate: https://docs.python.org/3/howto/logging-cookbook.html

Per uno sguardo approfondito al logging strutturato e a come può aiutare a rendere i log più informativi e più facili da analizzare, Loguru è ben documentato: https://loguru.readthedocs.io

Inoltre, considera di dare un'occhiata alla metodologia dell'app a 12 fattori, in particolare alla sezione sui log per la visione moderna del logging delle app: https://12factor.net/logs
