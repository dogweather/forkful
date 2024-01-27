---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) permette di separare i log degli errori dall'output principale. I programmatori lo usano per diagnosticare problemi senza intaccare i dati di output normali.

## How to:
In Kotlin, usa `System.err` per stampare su stderr. Semplice:

```Kotlin
fun main() {
    println("Output normale")
    System.err.println("Questo è un errore")
}
```

Output previsto:
```
Output normale
Questo è un errore
```

L'ordine dell'output può variare perché `System.out` e `System.err` usano buffer differenti.

## Deep Dive
Storicamente, la distinzione tra standard error e standard output risale ai tempi dei terminali Unix, aiutando nella reindirizzazione dei messaggi. Alcune alternative includono l'uso di logging frameworks come Logback o Log4j, i quali forniscono maggiore controllo e configurabilità. Nell'implementazione, `System.err` è un `PrintStream` e può essere reindirizzato o sostituito con uno custom se necessario.

## See Also
- [Documentazione ufficiale Kotlin](https://kotlinlang.org/docs/reference/)
- [Tutorial per il logging in Kotlin con Logback](https://www.baeldung.com/kotlin/logging)
