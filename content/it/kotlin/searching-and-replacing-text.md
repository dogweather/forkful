---
title:                "Cercare e sostituire testo"
html_title:           "Kotlin: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cosa e perché?

La ricerca e la sostituzione di testo è un processo comune tra i programmatori che consiste nel trovare una determinata sequenza di caratteri all'interno di un testo e sostituirla con un'altra sequenza di caratteri. Ciò è utile per automatizzare alcune operazioni di editing e risparmiare tempo. 

## Come fare:

```kotlin
// Esempio di ricerca e sostituzione
val testo = "Questo è un esempio di testo"
val nuovoTesto = testo.replace("esempio", "modello")

println(nuovoTesto)

// Output: Questo è un modello di testo
```

```kotlin
// Esempio di ricerca e sostituzione con espressione regolare
val testo = "Questo è un esempio di testo"
val nuovoTesto = testo.replace(Regex("[aeiou]"), "")

println(nuovoTesto)

// Output: Qst è n mpl d tst
```

## Approfondimento:

La ricerca e la sostituzione di testo è un'operazione comune nelle utility di editing di testo, come ad esempio Notepad++, che la implementa utilizzando espressioni regolari. Alcuni linguaggi di programmazione, come Python, hanno funzioni dedicate per la ricerca e la sostituzione di testo, mentre in altri, come C++, ci sono librerie di terze parti che lo permettono. Nella programmazione web, la ricerca e la sostituzione di testo è spesso utilizzata per manipolare i dati e generare dinamicamente il contenuto di una pagina.

## Vedi anche:

- [Documentazione Kotlin sulle espressioni regolari](https://kotlinlang.org/docs/regex.html)
- [Cheat sheet di Notepad++ sulle espressioni regolari](https://www.osmosys.asia/blog/regular-expression-search-replace-cheat-sheet/)
- [Libreria C++ per la ricerca e sostituzione di testo](https://github.com/phil0lucas/replace-latest)