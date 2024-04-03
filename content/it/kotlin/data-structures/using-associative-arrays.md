---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:46.060625-07:00
description: "Come fare: Creare e usare una mappa in Kotlin \xE8 semplice. Ecco una\
  \ guida rapida su come farlo."
lastmod: '2024-03-13T22:44:43.384583-06:00'
model: gpt-4-0125-preview
summary: "Creare e usare una mappa in Kotlin \xE8 semplice."
title: Utilizzo di array associativi
weight: 15
---

## Come fare:
Creare e usare una mappa in Kotlin è semplice. Ecco una guida rapida su come farlo:

```Kotlin
fun main() {
    // Creazione di una mappa mutabile
    val fruits = mutableMapOf("a" to "Mela", "b" to "Banana")

    // Aggiunta di elementi
    fruits["o"] = "Arancia" // Usando l'operazione di indicizzazione
    fruits.put("g", "Uva") // Usando il metodo put

    // Accesso agli elementi
    println(fruits["a"])  // Output: Mela
    println(fruits["b"])  // Output: Banana

    // Rimozione degli elementi
    fruits.remove("b")
    
    // Iterazione sulla mappa
    for ((key, value) in fruits) {
        println("$key -> $value")
    }
    // Esempio di output:
    // a -> Mela
    // o -> Arancia
    // g -> Uva
}
```

## Approfondimento
Le mappe di Kotlin derivano direttamente dalla loro interoperabilità con Java, dove le mappe sono una parte essenziale delle collezioni. Tuttavia, Kotlin ne migliora l’usabilità fornendo interfacce sia mutabili (`MutableMap`) che in sola lettura (`Map`), a differenza dell'interfaccia `Map` unificata di Java. Questa distinzione rende chiaro se una collezione è destinata alla modifica o meno.

Un dettaglio significativo riguardo l'implementazione della mappa in Kotlin è la distinzione esplicita tra mappe mutabili e immutabili, che sottolinea l'attenzione del linguaggio sull'immutabilità e sulla sicurezza dei thread.

Mentre le mappe sono molto utili, Kotlin offre anche altre collezioni come liste e insiemi, ciascuna con il proprio caso d'uso. Ad esempio, le liste mantengono l'ordine e consentono duplicati, rendendole ideali per l'accesso agli elementi tramite indice, mentre gli insiemi assicurano l'unicità ma non mantengono l'ordine. La scelta tra l'uso di una mappa, lista o insieme dipende dai requisiti specifici della tua applicazione, come la necessità di un accesso basato su chiavi o la conservazione dell’ordine.

Per quanto riguarda alternative migliori, se le prestazioni sono cruciali, specialmente con grandi collezioni, considera l'uso di strutture dati specializzate, più efficienti fornite da librerie esterne ottimizzate per casi d'uso specifici, come l’accesso simultaneo o l’ordinamento.
