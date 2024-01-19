---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La concatenazione delle stringhe è il processo di unire due o più stringhe insieme, termini chiave in Kotlin. I programmatori la usano per combinare informazioni di diversi tipi e formati, rendendola estremamente utile per la creazione di messaggi personalizzati o formattati.

## Come fare:
La concatenazione delle stringhe in Kotlin può essere effettuata usando il più (+), `plus()` metodo, o string template.

```
// Usando +
val stringa1 = "Ciao "
val stringa2 = "Mondo"
println(stringa1 + stringa2) // Output: Ciao Mondo

// Usando plus()
println(stringa1.plus(stringa2)) // Output: Ciao Mondo

// Usando string template
println("$stringa1$stringa2") // Output: Ciao Mondo
```

## Approfondimento
La concatenazione delle stringhe ha origini storiche che risalgono ai primi giorni dell'informatica, quando le risorse erano limitate e l'unione di stringhe era un procedimento costoso in termini di tempo. Con Kotlin, la concatenazione delle stringhe è resa semplice e veloce.

Possiamo usare il metodo `StringBuilder.append()` come alternativa per unire le stringhe. È più efficiente per le grandi operazioni di concatenazione perché non richiede la creazione di nuovi oggetti String per ogni operazione.

L'implementazione della concatenazione delle stringhe in Kotlin avviene internamente utilizzando `StringBuilder`. Quando concateni stinghe usando + o `, Kotlin li traduce in una serie di `append()` chiamate su `StringBuilder`. 

## Vedi Anche
- Documentazione ufficiale Kotlin: https://kotlinlang.org/docs/  
- Guida Kotlin per i programmatori Java: https://developer.android.com/kotlin/compare-to-java
- Kotlin Playground: per provare la concatenazione delle stringhe in Kotlin tu stesso: https://play.kotlinlang.org/