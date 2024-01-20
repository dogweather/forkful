---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Estrarre sottocatene (substring) significa prelevare una catena di caratteri all’interno di una più grande. I programmatori lo fanno per manipolare i dati di stringa in modo più granulare e controllato.

## Come fare:
Utilizzando il linguaggio di programmazione Kotlin, possiamo estrarre le sottocatene in vari modi. Ecco i metodi principali che Kotlin ci offre:

```Kotlin
val s = "Benvenuti a Kotlin"

// Estrarre usando gli indici
val substr1 = s.substring(0, 9)
println(substr1)  // Risultato: Benvenuti 

// Estrarre dall’inizio alla fine
val substr2 = s.substring(10..15)
println(substr2)  // Risultato: a Kotle
```

## Approfondimento:
(1) Estrazione di sottocatene non è un concetto nuovo. È stato una componente essenziale della programmazione da quando le stringhe sono state introdotte.

(2) Le alternative alla funzione substring sono slice e split. `slice` estrae una serie di caratteri da una stringa, mentre `split` divide una stringa in diverse sottocatene in base a un separatore.

(3) Kotlin implementa le funzioni di estrazione di sottocatene in formato overload, questo permette all'utente di passare gli intervalli o gli indici specifici da cui estrarre la sottocatena.

## Vedi anche:
- [Documentazione Ufficiale di Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html) per ulteriori dettagli su come usare le funzioni di estrazione delle sottocatene.
- [Discussione StackOverflow](https://stackoverflow.com/questions/36529972/how-do-i-get-a-substring-in-kotlin) su come ottenere una sottocatena in Kotlin.