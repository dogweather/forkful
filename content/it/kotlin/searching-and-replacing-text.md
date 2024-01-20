---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
La ricerca e la sostituzione del testo sono operazioni comuni in cui un programma trova una stringa specifica dentro un dato testo e, se la trova, la sostituisce con un'altra stringa. I programmatori lo fanno per manipolare e gestire i dati del testo in modo efficiente.

## Come si fa:
Il codice Kotlin seguente mostra come cercare e sostituire del testo in una stringa.

```kotlin
fun main() {
    var frase = "Amo Kotlin!"
    frase = frase.replace("Kotlin", "programmare")
    println(frase)
}
```
Uscita dell'esempio:
```output
Amo programmare!
```

## Analisi dettagliata:
La funzione `replace()` in Kotlin utilizzata per la ricerca e la sostituzione è stata introdotta dal linguaggio di programmazione Java, da cui Kotlin è derivato. Altre alternative includono l'uso di espressioni regolari o l'implementazione di funzioni personalizzate, sebbene `replace()` sia generalmente la più semplice e diretta.

In termini di dettaglio di implementazione, la funzione `replace()` funziona cercando la stringa di destinazione nell'oggetto String da sinistra a destra. Se la trova, la sostituisce con la nuova stringa fornita.

## Vedere anche:
Per una trattazione più profonda sulla ricerca e la sostituzione del testo in Kotlin si consiglia di consultare la documentazione ufficiale di Kotlin (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html).

Si potrebbe anche esplorare l'utilizzo delle espressioni regolari in Kotlin per la sostituzione del testo consultando (https://www.baeldung.com/kotlin-regex).