---
title:                "Registrazione Eventi (Logging)"
date:                  2024-01-26T01:00:13.528810-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione Eventi (Logging)"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/logging.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Effettuare il logging significa essenzialmente annotare ciò che il tuo programma sta facendo, tipicamente scrivendo messaggi su un file o su un terminale. I programmatori lo fanno per tenere traccia degli eventi, diagnosticare problemi e avere un registro di controllo che racconti la storia dell'operato di un'applicazione nel tempo.

## Come fare:
Cominciamo con alcune basi. C non ha un framework di logging incorporato, ma puoi crearne uno semplice con `stdio.h`. Ecco come:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // Rimuove il newline alla fine del risultato di ctime()
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("L'applicazione è stata avviata.");
    // ... il tuo codice va qui ...
    logMessage("L'applicazione sta facendo qualcosa di importante.");
    // ... il tuo codice continua ...
    logMessage("L'applicazione è terminata.");
    return 0;
}
```

Un esempio di output potrebbe essere questo:

```
[Tue Mar 9 12:00:01 2023] L'applicazione è stata avviata.
[Tue Mar 9 12:00:02 2023] L'applicazione sta facendo qualcosa di importante.
[Tue Mar 9 12:00:03 2023] L'applicazione è terminata.
```

Naturalmente, nel mondo reale probabilmente vorresti scrivere su un file anziché sul terminale, gestire diversi livelli di log e forse usare una libreria predefinita.

## Approfondimento
Il logging in C ha un fascino antico: è a basso livello come gran parte del resto del linguaggio. Storicamente, il logging veniva effettuato usando `fprintf` con `stderr` o un puntatore a file. Man mano che i programmi diventavano più complessi, anche le necessità di logging si ampliavano, portando allo sviluppo di librerie come `syslog` sui sistemi Unix, in grado di gestire il logging da molteplici fonti con vari livelli di importanza.

Nel panorama moderno, ci sono molte librerie di logging per il linguaggio C, come `zlog`, `log4c` e `glog`, che offrono un set di funzioni ricco, incluso la rotazione di log, il logging strutturato e il logging multithread. Queste soluzioni permettono un controllo fine sulla verbosità dei log, sulle destinazioni e sui formati.

Nell'implementare un sistema di logging, dettagli come la formattazione del timestamp, la gestione dei file di log e le prestazioni necessitano di considerazione. Apporre un timestamp ai log è fondamentale per correlare gli eventi, mentre la rotazione dei log assicura che i file di log non consumino troppo spazio su disco. L'azione di logging dovrebbe anche essere veloce e non bloccante rispetto al flusso principale dell'applicazione per prevenire che il logging diventi un collo di bottiglia.

## Vedi Anche
Per approfondire le librerie e le pratiche di logging in C, dai un'occhiata a queste risorse:

- Manuale GNU di `syslog`: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Una libreria di logging per C altamente configurabile - https://github.com/HardySimpson/zlog
- `log4c`: Un framework di logging per C modellato su Log4j - http://log4c.sourceforge.net/
- `glog`: La libreria di logging a livello applicativo di Google - https://github.com/google/glog
