---
title:                "Kotlin: Calcolare una data nel futuro o nel passato"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in molte situazioni, come ad esempio quando si desidera pianificare eventi o appuntamenti o quando si sta lavorando su progetti che richiedono un preciso programma temporale.

## Come fare

Per calcolare una data in Kotlin, possiamo utilizzare la classe `Calendar`. Innanzitutto, dobbiamo importare la libreria di Kotlin per la gestione delle date e dei tempi:

```Kotlin
import java.util.Calendar
```

Successivamente, possiamo creare un oggetto di tipo `Calendar` che rappresenti la data di oggi:

```Kotlin
val today = Calendar.getInstance()
```

Per calcolare una data nel futuro, possiamo utilizzare il metodo `add` associato alla classe `Calendar` e specificare la quantità di tempo da aggiungere (in questo caso, 1 mese):

```Kotlin
today.add(Calendar.MONTH, 1)
```

Per calcolare una data nel passato, possiamo utilizzare il metodo `add` con un valore negativo (in questo caso, 1 anno):

```Kotlin
today.add(Calendar.YEAR, -1)
```

Infine, per ottenere il valore della data calcolata, possiamo utilizzare il metodo `get` specificando il campo desiderato (ad esempio, `Calendar.DAY_OF_MONTH` per ottenere il giorno del mese) e convertire il valore in una stringa:

```Kotlin
val day = today.get(Calendar.DAY_OF_MONTH).toString()
```

Ecco un esempio completo di come calcolare una data nel futuro e nel passato e ottenere il risultato sotto forma di stringa:

```Kotlin
val today = Calendar.getInstance()

today.add(Calendar.MONTH, 1)
val futureDate = today.get(Calendar.DAY_OF_MONTH).toString()

today.add(Calendar.YEAR, -1)
val pastDate = today.get(Calendar.DAY_OF_MONTH).toString()

println("La data di oggi è: ${today.get(Calendar.DAY_OF_MONTH)}")
println("La data di oggi tra 1 mese sarà: $futureDate")
println("La data di oggi un anno fa era: $pastDate")
```

Output:

```
La data di oggi è: 25
La data di oggi tra 1 mese sarà: 25
La data di oggi un anno fa era: 25
```

## Approfondimento

La classe `Calendar` offre molte altre funzionalità per la gestione delle date e dei tempi, come ad esempio la possibilità di impostare una data specifica, di calcolare la differenza tra due date o di ottenere informazioni più dettagliate, come il numero della settimana in un determinato anno. Per approfondire queste funzionalità, ti consigliamo di consultare la documentazione ufficiale di Kotlin.

## Vedi anche

- Documentazione ufficiale di Kotlin sulle date e i tempi: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/