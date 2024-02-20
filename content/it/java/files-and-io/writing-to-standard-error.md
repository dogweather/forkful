---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:40.778268-07:00
description: "Scrivere su standard error (stderr) implica l'invio di messaggi di errore\
  \ e diagnostica alla console o al terminale. I programmatori lo fanno per separare\u2026"
lastmod: 2024-02-19 22:05:02.386251
model: gpt-4-0125-preview
summary: "Scrivere su standard error (stderr) implica l'invio di messaggi di errore\
  \ e diagnostica alla console o al terminale. I programmatori lo fanno per separare\u2026"
title: Scrivere sull'errore standard
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere su standard error (stderr) implica l'invio di messaggi di errore e diagnostica alla console o al terminale. I programmatori lo fanno per separare le informazioni di errore dall'output standard (stdout), facilitando il debugging e l'analisi dei log.

## Come fare:

### Output di Base su stderr in Java
Java offre un modo semplice per scrivere su stderr utilizzando `System.err.print()` o `System.err.println()`. Ecco come si fa:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Errore: Non è possibile dividere per zero.");
        }
    }
}
```

Output di esempio:

```
Errore: Non è possibile dividere per zero.
```

Questo stamperà direttamente il messaggio di errore sul flusso di errore standard.

### Utilizzo di un Logger per una Gestione Avanzata degli Errori
Per applicazioni che necessitano di una gestione degli errori e log più sofisticata, è comune utilizzare una libreria di logging come SLF4J con Logback o Log4J2. Questo permette una maggiore flessibilità nella gestione dell'output degli errori, inclusa la reindirizzazione su file, il filtraggio e la formattazione.

#### Esempio con Logback

Prima, aggiungere la dipendenza per Logback al proprio file `pom.xml` (Maven) o `build.gradle` (Gradle). Per Maven:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

Quindi, si può utilizzare il seguente codice per registrare gli errori:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Errore: Non è possibile dividere per zero.", e);
        }
    }
}
```

Questo produrrà l'output del messaggio di errore insieme a una stack trace sulla console o su un file, a seconda della configurazione di Logback.

Usare framework di logging come Logback offre un maggiore controllo sulla gestione degli errori, rendendo più semplice la gestione di applicazioni e sistemi di grandi dimensioni.
