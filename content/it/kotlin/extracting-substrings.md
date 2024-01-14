---
title:                "Kotlin: Estrazione di sottostringhe"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è un'importante funzionalità nella programmazione Kotlin che consente di ottenere una porzione di una stringa più grande. Ciò è particolarmente utile quando si lavora con dati strutturati e si vuole estrarre informazioni specifiche da una stringa.

## Come utilizzare l'estrazione di sottostringhe in Kotlin

Per utilizzare questa funzionalità, basta usare il metodo `substring()` su una stringa e specificare l'indice di inizio e fine della sottostringa desiderata. Ad esempio:

```Kotlin
val str = "Benvenuti in questo blog post"

// Estrarre la parola "blog"
val blog = str.substring(18, 22)

// Stampa "blog"
println(blog)
```

Questa funzione è molto utile quando si lavora con stringhe di lunghezza variabile e si desidera accedere a una parte specifica di esse.

## Approfondimento sull'estrazione di sottostringhe

Il metodo `substring()` di Kotlin ha tre varianti: `substring(beginIndex)`, `substring(beginIndex, endIndex)` e `substring(range)`. La variante con un solo parametro indica l'indice di inizio della sottostringa e restituisce la parte della stringa dalla posizione specificata fino alla fine della stringa. La variante a due parametri indica l'indice di inizio e di fine della sottostringa e restituisce la porzione della stringa compresa tra questi due indici. Infine, la variante con un parametro di tipo range indica un intervallo di indici e restituisce la sottostringa corrispondente.

Inoltre, è possibile utilizzare il metodo `substringAfter()` per ottenere la parte della stringa che si trova dopo un determinato delimitatore, e `substringBefore()` per ottenere la parte che si trova prima. Ci sono anche altre funzioni disponibili come `substringBeforeLast()` e `substringAfterLast()` che lavorano con gli ultimi delimitatori nelle stringhe.

## Vedi anche

Per ulteriori informazioni su come lavorare con le stringhe in Kotlin, consulta la documentazione ufficiale [qui](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/). Puoi anche trovare altri esempi e tutorial su [Kotlin Tutorial](https://www.programiz.com/kotlin-programming/substring). Buon coding!