---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Kotlin: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Cosa e Perché?

La funzione di calcolo della data nel futuro o nel passato è un'operazione comune nella programmazione. Permette ai programmatori di ottenere una data specifica in base ad un determinato numero di giorni, settimane o mesi in avanti o indietro. Questa funzione è utile in situazioni come pianificazione di eventi futuri o gestione delle scadenze dei progetti.

# Come fare:

Per calcolare una data nel futuro o nel passato in Kotlin, è possibile utilizzare la funzione `add` della classe `Calendar`. Di seguito è presente un esempio di codice che calcola la data di oggi e la data tra tre mesi:

```Kotlin
val today = Calendar.getInstance()
val futureDate = today.clone() as Calendar
futureDate.add(Calendar.MONTH, 3)

println("Oggi è il ${today.get(Calendar.DAY_OF_MONTH)}/${today.get(Calendar.MONTH)}/${today.get(Calendar.YEAR)}")
println("La data tra tre mesi sarà il ${futureDate.get(Calendar.DAY_OF_MONTH)}/${futureDate.get(Calendar.MONTH)}/${futureDate.get(Calendar.YEAR)}")
```
Output:
```
Oggi è il 21/9/2021
La data tra tre mesi sarà il 21/12/2021
```

# Approfondimento:

**Contesto storico:** In passato, il calcolo di una data nel futuro o nel passato era molto più complesso e richiedeva l'utilizzo di algoritmi complessi. Con l'avvento della programmazione e dei linguaggi di programmazione ad alto livello come Kotlin, è diventato molto più semplice e rapido eseguire questa operazione.

**Alternative:** Oltre all'utilizzo della funzione `add` della classe `calendar`, è possibile utilizzare la libreria esterna `Joda-Time` oppure la nuova API `java.time` introdotta in Java 8 per il calcolo di date nel futuro o nel passato.

**Dettagli di implementazione:** La funzione `add` accetta due parametri: il primo è il campo di data (giorno, mese o anno) da modificare e il secondo è il valore da aggiungere. Ad esempio, `add(Calendar.MONTH, 3)` aggiunge tre mesi alla data corrente. Inoltre, è possibile utilizzare valori negativi per ottenere date nel passato.

# Vedi anche:

- Documentazione ufficiale di Kotlin sulle funzioni di manipolazione della data: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-calendar/add.html
- Documentazione di Java sulle funzioni di manipolazione della data: https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html#add-int-int-
- Tutorial su come utilizzare la funzione `add` in Kotlin: https://www.baeldung.com/kotlin/date-time-api