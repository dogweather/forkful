---
title:                "Scrivere su errore standard"
html_title:           "Kotlin: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere a standard error è un'operazione fondamentale nella programmazione in Kotlin. Essa permette di gestire gli errori e le eccezioni nel codice in maniera efficace, migliorando la qualità e la sicurezza delle nostre applicazioni.

## Come fare

Per scrivere a standard error in Kotlin, possiamo utilizzare la funzione `System.err.println()`. Questa funzione accetta una stringa come parametro e la stampa sulla console degli errori. Ecco un esempio di come utilizzarla:

```Kotlin
System.err.println("Errore: il valore inserito non è valido.")
```

Output:

```
Errore: il valore inserito non è valido.
```

## Approfondimento

La console degli errori, o standard error, è un canale di output separato dalla console standard di un programma. Questo canale viene utilizzato per mostrare messaggi di errore, avvisi e altre informazioni importanti durante l'esecuzione di un programma.

Per scrivere a standard error, Kotlin utilizza la classe `System` e il suo attributo `err`, rappresentato dall'oggetto di tipo `PrintStream`. Possiamo quindi utilizzare tutte le funzioni disponibili in `PrintStream` per formattare e gestire i messaggi di errore.

Un'altra funzione utile è `error()`, che permette di lanciare un'eccezione specificando un messaggio di errore. Ad esempio:

```Kotlin
if (username.isEmpty()) {
    System.err.error("Il nome utente non può essere vuoto.")
}
```

## Vedi anche
- [Documentazione ufficiale di Kotlin su System](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.lang.-system/index.html)
- [Guida introduttiva su Ardit](https://www.ardit.it/come-gestire-gli-errori-in-kotlin/)
- [Tutorial su Vogella](https://www.vogella.com/tutorials/Kotlin/article.html#error-handling-and-exceptions)