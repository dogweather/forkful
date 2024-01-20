---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?

Creare un file temporaneo significa generare un file che serve solo per un breve periodo di tempo. Solitamente, lo utilizziamo per memorizzare dati che non abbiamo bisogno di conservare a lungo termine, ma che sono necessari per un brevissimo lasso di tempo nel nostro flusso di lavoro.

## Come Fare:

Creare un file temporaneo in Kotlin è semplice come questo:

```kotlin
import java.nio.file.Files.createTempFile

fun main() {
    val tempFile = createTempFile("temp", ".txt")
    println("File temporaneo creato: ${tempFile.toAbsolutePath()}")
}
```

Se lo esegui, vedrai qualcosa simile a:

```
File temporaneo creato: /tmp/temp123456-7890.txt
```

## Approfondimento

La creazione di file temporanei è pratica comune fin dai primi giorni della programmazione. Nel sistema operativo Unix originale, ad esempio, i file temporanei erano spesso utilizzati per evitare di riempire la memoria con dati di lavoro.

Un'alternativa alla creazione di un file temporaneo è l'utilizzo di un'area di memoria temporanea, o buffer. Un buffer mantiene I dati in memoria invece che su disco, ma non è la scelta migliore per grandi quantità di dati o dati che devono persistere tra esecuzioni separate.

Riguardo ai dettagli di implementazione, il metodo `createTempFile` di Java, che usiamo nell'esempio precedente, crea effettivamente il file temporaneo nel percorso specificato, o nel percorso di sistema predefinito se non ne viene specificato uno. Il nome del file temporaneo inizia con il prefisso fornito e si conclude con il suffisso fornito.

## Vedi Anche

Per maggiori informazioni e per scoprire funzionalità correlate, puoi fare riferimento a queste risorse:

- Documentazione ufficiale di Kotlin: [https://kotlinlang.org/docs/reference/](https://kotlinlang.org/docs/reference/)
- Funzionalità dei file in Kotlin: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- Metodi per la manipolazione dei file in Java: [https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)