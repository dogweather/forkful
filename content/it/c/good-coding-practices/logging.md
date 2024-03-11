---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:23.868581-07:00
description: "Il logging in C comporta la registrazione del flusso e degli eventi\
  \ notevoli di un programma durante il suo runtime, fornendo una revisione tangibile\
  \ del\u2026"
lastmod: '2024-03-11T00:14:17.534477-06:00'
model: gpt-4-0125-preview
summary: "Il logging in C comporta la registrazione del flusso e degli eventi notevoli\
  \ di un programma durante il suo runtime, fornendo una revisione tangibile del\u2026"
title: Registro degli Eventi
---

{{< edit_this_page >}}

## Cosa & Perché?

Il logging in C comporta la registrazione del flusso e degli eventi notevoli di un programma durante il suo runtime, fornendo una revisione tangibile del suo comportamento e delle sue prestazioni. I programmatori utilizzano il logging per scopi di debug, monitoraggio della salute del software e garanzia della sicurezza del sistema.

## Come fare:

In C, il logging può essere ottenuto con operazioni su file di base o utilizzando librerie più sofisticate. Per semplicità, inizieremo con la libreria standard di I/O. I seguenti frammenti mostrano implementazioni di base del logging.

Per registrare messaggi semplici:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // Apri il file di log in modalità di append
    
    if (logFile == NULL) {
        perror("Errore nell'apertura del file di log.");
        return -1;
    }
    
    fprintf(logFile, "Avvio applicazione.\n");
    
    // La logica della tua applicazione qui
    
    fprintf(logFile, "Applicazione terminata con successo.\n");
    fclose(logFile);
    
    return 0;
}
```

Output in `application.log`:

```
Avvio applicazione.
Applicazione terminata con successo.
```

Per includere log più dettagliati con timestamp e livelli di log:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // Rimuovi il carattere di nuova linea
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("Errore nell'apertura del file di log.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "Avvio applicazione");
    // La logica della tua applicazione qui
    logMessage(logFile, "ERROR", "Un esempio di errore");
    
    fclose(logFile);
    
    return 0;
}
```

Output in `detailed.log`:

```
[Gio Mar 10 14:32:01 2023] INFO - Avvio applicazione
[Gio Mar 10 14:32:02 2023] ERROR - Un esempio di errore
```

## Approfondimento

Come dimostrato, il logging in C si basa su semplici operazioni su file, che è efficace ma non potente o flessibile quanto le facilities di logging in altri linguaggi, come il modulo `logging` di Python o `Log4j` di Java. Per capacità di logging più avanzate in C, gli sviluppatori spesso si rivolgono a librerie come `syslog` su sistemi simili a Unix, che fornisce la gestione dei log a livello di sistema, o librerie di terze parti come `log4c`.

Storicamente, il logging è stato una parte integrante della programmazione, risalente alle prime pratiche di programmazione dove il tracciamento e la comprensione del flusso del programma e degli errori erano principalmente fatti attraverso stampe fisiche. Con l'evoluzione dei sistemi, il logging è diventato più sofisticato, supportando ora vari livelli di gravità, rotazione dei log e logging asincrono.

Sebbene la libreria standard di C fornisca gli strumenti di base per implementare il logging, le sue limitazioni spesso portano alla creazione di framework di logging personalizzati o all'adozione di librerie esterne per soluzioni di logging più ricche di funzionalità e flessibili. Nonostante queste limitazioni, comprendere e implementare il logging di base in C è cruciale per il debug e la manutenzione del software, specialmente in ambienti in cui è necessario minimizzare le dipendenze esterne.
