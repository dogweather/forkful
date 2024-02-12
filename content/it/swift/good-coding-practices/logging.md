---
title:                "Registrazione delle Attività (Logging)"
aliases:
- /it/swift/logging.md
date:                  2024-01-26T01:09:02.639766-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione delle Attività (Logging)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/logging.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Il logging è il processo di registrazione dei comportamenti delle applicazioni, degli errori e di altre informazioni importanti su un supporto persistente, come un file o un database. I programmatori lo fanno per tracciare la salute e le prestazioni delle loro app, per risolvere problemi e per tenere d'occhio quello che succede sotto il cofano negli ambienti di produzione.

## Come fare:
In Swift, puoi scrivere log sulla console con istruzioni di stampa o con l'API `os.log`, più flessibile, che si interconnette con il Sistema di Logging Unificato sulle piattaforme Apple.

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // Semplice istruzione di stampa
    print("Inizio del fetch")
    
    // Registrazione di un evento a livello di informazioni utilizzando os.log
    os_log(.info, log: logger, "Recupero dati dall'API.")
    
    do {
        let data = try performNetworkRequest()
        // Registrazione di un evento a livello di debug
        os_log(.debug, log: logger, "Dati ricevuti: %@", data.description)
    } catch {
        // Registrazione di un evento a livello di errore
        os_log(.error, log: logger, "Impossibile recuperare i dati: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Simulazione di una richiesta di rete
    return Data()
}
```

Un esempio di output sulla console potrebbe apparire così:

```
Inizio del fetch
Recupero dati dall'API.
Dati ricevuti: Alcuni byte di dati...
```

Per gli errori, potrebbe essere:

```
Impossibile recuperare i dati: La connessione Internet sembra essere offline.
```

## Approfondimento
Il logging in Swift assume nuova potenza ed efficienza con il Sistema di Logging Unificato introdotto in iOS 10 e macOS Sierra. A differenza dell'istruzione `print` che va direttamente in console, questo sistema è basato sulle attività e consente di filtrare i messaggi di log in base alla loro importanza e se sono build di debug o di rilascio.

Il contesto storico inquadra l'evoluzione del logging in iOS e macOS, passando da semplici istruzioni di stampa verso strumenti comprensivi che si integrano con l'app Strumenti e Console, fornendo modi sofisticati per analizzare i log.

Ci sono una serie di alternative al logging all'interno di Swift, come librerie di terze parti come CocoaLumberjack, che offre uno strato macro sopra il Sistema di Logging Unificato. Fornisce maggiore controllo sul formato dei log, sulla gestione dei file e sulle opzioni di prestazione.

Infine, i dettagli di implementazione; OSLog è progettato non solo per essere efficiente ma anche attento alla privacy, con la capacità di oscurare i dati privati quando si effettua il logging. Categorizza i log in livelli di fault, errore, informazioni e debug, ognuno offrendo una diversa granularità per la risoluzione dei problemi.

## Vedi anche
- [Documentazione di Apple sul sistema di Logging Unificato](https://developer.apple.com/documentation/os/logging)
- [Tutorial di Ray Wenderlich sul logging in Swift con os.log](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [Repository GitHub di CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)
