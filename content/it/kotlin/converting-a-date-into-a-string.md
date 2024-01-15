---
title:                "Convertire una data in una stringa"
html_title:           "Kotlin: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui possiamo avere bisogno di convertire una data in una stringa. Forse vogliamo mostrare la data in un formato specifico, o forse vogliamo salvare la data come parte di una stringa più lunga in un database. In ogni caso, la capacità di convertire una data in una stringa è un'abilità importante da avere quando si programma in Kotlin.

## Come fare

Per convertire una data in una stringa, dobbiamo prima creare un oggetto di tipo `LocalDate` utilizzando la libreria standard di Kotlin. Una volta creato l'oggetto, possiamo utilizzare il metodo `format()` passando come argomento un oggetto `DateTimeFormatter` per specificare il formato in cui vogliamo visualizzare la data.

Ecco un esempio di codice che converte la data corrente in una stringa nel formato "dd/MM/yyyy":

```Kotlin
val currentDate = LocalDate.now()
val dateFormat = DateTimeFormatter.ofPattern("dd/MM/yyyy")
val formattedDate = currentDate.format(dateFormat)

println(formattedDate) // Output: 20/07/2021
```

È anche possibile utilizzare un oggetto `SimpleDateFormat` per formattare la data in un formato specifico:

```Kotlin
val currentDate = LocalDate.now()
val dateFormat = SimpleDateFormat("dd/MM/yyyy")
val formattedDate = dateFormat.format(currentDate)

println(formattedDate) // Output: 20/07/2021
```

Entrambe le soluzioni restituiscono lo stesso risultato, ma utilizzano sintassi leggermente differenti. È importante notare che il formato specificato deve essere corrispondente alla data, altrimenti verrà lanciata un'eccezione.

## Approfondimento

Nella programmazione, le date sono rappresentate da numeri. Ad esempio, nella libreria standard di Kotlin, una data è rappresentata dal numero di giorni trascorsi dalla "Epoch", che è il 1° gennaio 1970. Quando chiamiamo il metodo `format()` per convertire una data in una stringa, stiamo effettivamente facendo una serie di operazioni matematiche per ottenere il formato desiderato.

Esistono molti modi per formattare una data in una stringa, e il modo più corretto dipenderà dal contesto in cui si sta lavorando. Se siamo interessati a una formattazione locale, possiamo utilizzare la classe `DateTimeFormatter` di Kotlin, mentre se vogliamo un controllo più preciso sul formato, possiamo utilizzare `SimpleDateFormat` di Java.

## Vedi anche

- [Documentazione ufficiale di Kotlin per la classe LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Tutorial su come formattare le date in Kotlin](https://www.baeldung.com/kotlin-datetime-format)
- [Documentazione ufficiale di Java per la classe SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)