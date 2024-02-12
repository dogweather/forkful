---
title:                "Registrazione delle Attività (Logging)"
aliases: - /it/kotlin/logging.md
date:                  2024-01-26T01:06:52.431830-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registrazione delle Attività (Logging)"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/logging.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La registrazione (logging), in sostanza, è la pratica di annotare eventi e dati da un'applicazione software su un output esterno, come un file o la console. I programmatori effettuano registrazioni per tracciare il percorso del codice, per risolvere problemi e per tenere d'occhio il comportamento di un'app nell'ambiente reale, fornendo intuizioni critiche che non si possono ottenere altrettanto efficacemente in nessun altro modo.

## Come fare:

In Kotlin, la registrazione può essere effettuata utilizzando la funzione integrata `println()` per casi semplici, o con librerie più sofisticate come SLF4J con Logback o Log4j per esigenze avanzate.

Di seguito è riportato un esempio basilare che utilizza `println()`:

```Kotlin
fun main() {
    println("Messaggio di log semplice: Applicazione avviata.")
    // ... qui la logica dell'applicazione ...
    try {
        // Simula un errore
        throw Exception("Errore simulato")
    } catch (e: Exception) {
        println("Messaggio di log d'errore: " + e.message)
    }
}
```

Output:
```
Messaggio di log semplice: Applicazione avviata.
Messaggio di log d'errore: Errore simulato
```

Ed ecco un frammento che utilizza SLF4J con Logback configurato:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Messaggio di log strutturato: App lanciata.")
    // ... qui la logica dell'applicazione ...
    try {
        // Simula un errore
        throw Exception("Errore simulato")
    } catch (e: Exception) {
        logger.error("Log strutturato d'errore: ", e)
    }
}
```

Assumendo la configurazione appropriata di Logback, l'output verrebbe formattato e potrebbe apparire più o meno così quando scritto in un file di log:
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Messaggio di log strutturato: App lanciata.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Log strutturato d'errore: 
java.lang.Exception: Errore simulato
   at com.myapp.Main.main(Main.kt:10)
```

## Approfondimento

Storicamente, la registrazione nel software si è sviluppata insieme alla crescente complessità delle applicazioni e dei sistemi. Semplici istruzioni di stampa erano sufficienti nei primi giorni, quando i programmi venivano spesso eseguiti e corretti dagli stessi sviluppatori. Ma man mano che i sistemi si sono interconnessi ed eseguiti in ambienti diversi da utenti differenti, un sistema di registrazione robusto e persistente è diventato cruciale.

Prima che Kotlin diventasse popolare, gli sviluppatori Java adottarono ampiamente librerie come Log4j e, successivamente, SLF4J. Queste hanno ispirato pratiche simili in Kotlin, sfruttando l'interoperabilità di Kotlin con le librerie Java. SLF4J agisce come uno strato di astrazione, consentendo di sostituire l’implementazione effettiva di logging—di solito Logback o Log4j2 sono le scelte preferite.

Kotlin consente anche soluzioni di registrazione multi-piattaforma che funzionano su JVM, JavaScript e Native, ad esempio, attraverso il meccanismo `expect`/`actual`, che astrae le implementazioni specifiche della piattaforma.

In contrasto con le librerie di logging dedicate, println persiste come la forma più semplice di registrazione perché non richiede configurazioni aggiuntive o dipendenze; tuttavia, di solito è inadatta per applicazioni in produzione a causa della sua mancanza di funzionalità come i livelli di log, la rotazione dei log e i formati strutturati.

Altre caratteristiche comuni dei framework di registrazione avanzati includono:

- Livelli di log (DEBUG, INFO, WARN, ERROR, etc.) per categorizzare l'urgenza dei messaggi di log.
- Output verso vari destinatari, come console, file, database o servizi di rete.
- Rotazione automatica dei log e politiche di conservazione.
- Supporto per la tracciatura distribuita per l'architettura a microservizi.
- Logging strutturato utilizzando formati come JSON, che si integra bene con i sistemi di analisi dei log.

Questi strumenti e funzionalità sono critici per mantenere un sistema affidabile e osservabile, specialmente in ambienti complessi, distribuiti o ad alta scalabilità.

## Vedi Anche

Per ulteriori studi e approfondimenti sul logging in Kotlin, controlla:

- SLF4J (Simple Logging Facade for Java) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, il successore di Log4j [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Documentazione Kotlin Multiplatform sulle dichiarazioni 'expect' e 'actual': [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Una guida al logging strutturato in Kotlin: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
