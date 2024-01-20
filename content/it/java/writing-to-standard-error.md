---
title:                "Scrivere sull'errore standard"
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) è utile per segnalare errori o avvisi. Distinguiamolo dall'output standard (stdout) per analisi e debugging efficaci.

## How to:
```Java
public class StdErrExample {
    public static void main(String[] args) {
        System.out.println("Questo va sull'output standard");
        System.err.println("Questo va sull'errore standard");
    }
}
```
Output:
```
Questo va sull'output standard
Questo va sull'errore standard
```

## Deep Dive
Prima del 1982, con l'introduzione di Unix System III, stderr non esisteva. Significava che output e errori condividevano la stessa via. Oggi, invece, separare stdout da stderr permette di reindirizzare e controllare l'output più facilmente. Alternative includono file di logging o librerie di logging (es. SLF4J, Logback). Internamente, `System.err` è un oggetto di tipo `PrintStream` che punta al flusso di errore standard del sistema operativo.

## See Also
- [Documentazione ufficiale Oracle su `System.err`](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Guida sulla I/O streams in Java](https://www.baeldung.com/java-io)
- [Logging in Java con SLF4J](https://www.slf4j.org/manual.html)