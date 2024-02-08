---
title:                "Registrazione delle Attività (Logging)"
aliases:
- it/java/logging.md
date:                  2024-01-26T01:06:55.569981-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione delle Attività (Logging)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/logging.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Il logging è essenzialmente il processo di registrazione degli eventi che si verificano all'interno di una applicazione software. I programmatori registrano questi eventi per catturare informazioni in tempo di esecuzione, per diagnosticare problemi, monitorare il comportamento del sistema, e creare una traccia di verifica per scopi di sicurezza e conformità.

## Come fare:
Ecco un modo semplice per iniziare con il logging in Java utilizzando il pacchetto integrato `java.util.logging`.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Registrazione di un messaggio a livello INFO");

        try {
            int divisione = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Si è verificata un'eccezione", e);
        }
    }
}
```

Questo produrrà un'uscita simile a:

```
03 lug 2023 14:00:00 PM AppLogging main
INFO: Registrazione di un messaggio a livello INFO
03 lug 2023 14:00:00 PM AppLogging main
SEVERE: Si è verificata un'eccezione
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## Approfondimenti
Il logging in Java si è notevolmente evoluto. Storicamente, il logging era più ad-hoc con uscite di sistema e meccanismi scritti autonomamente. Tuttavia, la necessità di standardizzazione ha portato alle API di logging come `Log4j` e `SLF4J`. Il pacchetto `java.util.logging` è stato introdotto in JDK 1.4, fornendo un modo standardizzato per registrare i messaggi.

Alternative a `java.util.logging` (JUL) includono Log4j 2 e SLF4J. Mentre JUL è integrato in Java e quindi non richiede dipendenze aggiuntive, sia Log4j 2 che SLF4J offrono funzionalità più avanzate come un controllo più granulare sulla configurazione del logging, logging asincrono e migliori prestazioni.

Dal punto di vista dell'implementazione, il logging può essere sincrono, dove ogni messaggio di log viene elaborato nel thread che lo ha generato, o asincrono, dove i messaggi vengono passati a un thread separato. Il logging asincrono può migliorare le prestazioni ma introduce complessità poiché si deve gestire la concorrenza e assicurare che i messaggi di log non vengano persi in caso di crash dell'applicazione.

## Vedi anche
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Panoramica ufficiale del logging di Oracle](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Tutorial su java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
