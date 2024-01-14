---
title:                "Kotlin: Capitalizzare una stringa"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è un'operazione comune quando si lavora con le applicazioni che gestiscono i dati dei clienti. Può essere utile per uniformare il formato dei nomi o dei dati inseriti dall'utente, o semplicemente per rendere più leggibili i dati durante la stampa.

## Come Fare

Per capitalizzare una stringa in Kotlin, possiamo utilizzare la funzione `capitalize()` che fa parte della classe String. Questa funzione prende la prima lettera della stringa e la trasforma in maiuscolo, mantenendo inalterate le altre lettere.

```Kotlin
val nome = "mario"
val nomeCapitalizzato = nome.capitalize()
println(nomeCapitalizzato) // Output: Mario
```

Possiamo anche capitalizzare solo la prima lettera di ogni parola all'interno di una stringa utilizzando la funzione `capitalizeEachWord()`.

```Kotlin
val frase = "ciao a tutti"
val fraseCapitalizzata = frase.capitalizeEachWord()
println(fraseCapitalizzata) // Output: Ciao A Tutti
```

Queste funzioni sono molto utili per la formattazione dei dati e possono essere applicate a qualsiasi stringa.

## Approfondimento

Se vogliamo comprendere meglio come funzionano le funzioni `capitalize()` e `capitalizeEachWord()`, possiamo dare un'occhiata all'implementazione all'interno della classe String.

La funzione `capitalize()` utilizza il metodo `substring()` per prendere la prima lettera della stringa e il metodo `toUpperCase()` per trasformarla in maiuscolo. Dopodiché, utilizza il metodo `substring()` per aggiungere il resto della stringa.

La funzione `capitalizeEachWord()` usa le stesse tecniche, ma con un ciclo `forEach()` per applicare il processo a ogni parola all'interno della stringa.

Questo ci mostra che le funzioni `capitalize()` e `capitalizeEachWord()` sono solo alcune delle molte funzionalità utili che la classe String di Kotlin offre per manipolare le stringhe.

## Vedi Anche

- Documentazione String Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/
- Approfondimenti su metodi di manipolazione delle stringhe: https://kotlinexpertise.com/convert-string-to-int-kotlin/