---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

L'eliminazione di caratteri che corrispondono a un determinato modello è una pratica in programmazione utilizzata per filtrare o manipolare stringhe di testo. I programmatori lo fanno per pulire i dati in ingresso, regolare il formato del testo e semplificare le operazioni successive.

## Come si fa:

Ecco un esempio semplice in Kotlin:

```Kotlin
fun main(args: Array<String>) {
  val regexPattern = "[^a-zA-Z]".toRegex()
  val inputString = "12345Ciao, Mondo!67890"
  val resultString = inputString.replace(regexPattern, "")
  println(resultString)
}
```

L'output sarebbe:

```
CiaoMondo
```

## Approfondimento

L'uso di espressioni regolari (o regex) per l'eliminazione di caratteri corrispondenti a un modello ha una lunga storia in informatica, spaziando da shells Unix agli attuali linguaggi ad oggetti. In Kotlin, il metodo `replace()` di una stringa può essere combinato con un modello regex per raggiungere questa funzione.

Esistono molte alternative all'uso del metodo `replace()`, come iterare tra i caratteri di stringa o utilizzare librerie di terze parti con funzionalità specializzate.

Dal punto di vista dell'implementazione, il metodo `replace()` di Kotlin utilizza internamente il motore di regex di Java, sfruttando le sue efficienti strutture dati e algoritmi per l'elaborazione di espressioni regolari.

## Vedi Anche

- Kotlin String API: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/
- Documentazione Java Regex: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html
- Introduzione alle espressioni regolari (regex): https://developer.mozilla.org/it/docs/Web/JavaScript/Guida/Espressioni_regolari