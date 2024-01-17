---
title:                "Cancellazione dei caratteri corrispondenti a un modello"
html_title:           "Kotlin: Cancellazione dei caratteri corrispondenti a un modello"
simple_title:         "Cancellazione dei caratteri corrispondenti a un modello"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?
Eliminare i caratteri corrispondenti a un determinato modello è una tecnica utilizzata dai programmatori per rimuovere parti indesiderate da una stringa di testo. Questo può essere utile per ripulire dati o per eseguire operazioni più avanzate su una stringa di testo.

## Come eseguire: 
Di seguito sono riportati due esempi di codice in Kotlin per eliminare i caratteri corrispondenti a un pattern specifico:

```
val stringa = "Hello, world!"
val nuovoStringa = stringa.replace(Regex("[H, l]"), "")

println(nuovoStringa)
// Output: eo, word!
```

```
val stringa = "123abc456"
val nuovoStringa = stringa.filter { it.isDigit() }

println(nuovoStringa)
// Output: 123456
```

## Approfondimento:
La rimozione dei caratteri corrispondenti a un determinato pattern è una tecnica che ha origini nel linguaggio di programmazione Perl, noto per le sue potenti funzionalità di elaborazione delle stringhe. Un altro modo per eliminare caratteri in Kotlin è utilizzando il metodo `trim()` che rimuove tutti gli spazi vuoti all'inizio e alla fine di una stringa. Per operazioni più complesse, è possibile utilizzare espressioni regolari per trovare e sostituire i caratteri corrispondenti a un determinato pattern.

## Vedi anche:
- [Documentazione ufficiale di Kotlin sulla rimozione di caratteri](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/filter.html)
- [Guida alla manipolazione delle stringhe con espressioni regolari in Kotlin](https://medium.com/@burhanrashid52/string-manipualtion-with-regular-expresions-in-kotlin-985598639b4b)