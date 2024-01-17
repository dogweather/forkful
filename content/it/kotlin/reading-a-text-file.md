---
title:                "Leggere un file di testo"
html_title:           "Kotlin: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lettura di un file di testo è il processo di aprire e leggere un file che contiene testo o stringhe di caratteri. I programmatori spesso fanno questo per manipolare o elaborare dati contenuti nel file di testo.

## Come fare:
```kotlin
// leggere il file di testo
val file = File("nome_file.txt")
val lines = file.readLines()

// ciclo per stampare ogni riga
for (line in lines) {
  println(line)
}
```

Esempio di file di testo (```nome_file.txt```):
```
Ciao!
Come stai?
Spero che tutto vada bene.
```

Output:
```
Ciao!
Come stai?
Spero che tutto vada bene.
```

## Approfondimento:
La lettura di un file di testo è una delle operazioni più comuni in programmazione, poiché permette ai programmatori di accedere e manipolare dati esterni ai loro programmi. È possibile leggere un file di testo utilizzando diverse metodologie, come ad esempio utilizzando librerie di terze parti come Apache Commons IO o utilizzando metodi built-in di linguaggi di programmazione. È importante considerare anche l'encoding del file, poiché può influire sulla corretta lettura dei caratteri speciali.

## Vedi anche:
- [Documentazione su come leggere un file di testo in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Tutorial su come leggere un file di testo in Java utilizzando Apache Commons IO](https://www.baeldung.com/java-read-file)