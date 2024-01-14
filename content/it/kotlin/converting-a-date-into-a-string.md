---
title:    "Kotlin: Convertire una data in una stringa"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa è una pratica comune nella programmazione Kotlin, poiché ci consente di rappresentare la data in un formato leggibile per gli utenti. Inoltre, ci permette di manipolare e gestire le date in modo più flessibile all'interno del nostro codice.

## Come Fare

Per convertire una data in una stringa in Kotlin, possiamo utilizzare il metodo `format` della classe `DateTimeFormatter`.

```
```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val date = LocalDate.now()
val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
val dateString = date.format(formatter)
println(dateString) // output: 20/10/2021
```
```

Possiamo specificare il formato della stringa utilizzando le lettere corrispondenti ai diversi componenti della data, come ad esempio `dd` per il giorno, `MM` per il mese o `yyyy` per l'anno.

Possiamo anche utilizzare questa stessa logica per convertire un oggetto `Date` in una stringa:

```
```Kotlin
import java.util.Date
import java.text.SimpleDateFormat

val date = Date()
val formatter = SimpleDateFormat("dd/MM/yyyy")
val dateString = formatter.format(date)
println(dateString) // output: 20/10/2021
```
```

## Approfondimento

La classe `DateTimeFormatter` in Kotlin offre molte opzioni per formattare le date in modo personalizzato. Oltre ai caratteri utilizzati nell'esempio precedente, possiamo anche utilizzare `hh` per le ore, `mm` per i minuti e `ss` per i secondi.

Inoltre, possiamo sfruttare il metodo `ofLocalizedDate` per ottenere una stringa formattata in base alle impostazioni regionali del nostro sistema.

## Vedi Anche

- Documentazione di Kotlin su la conversione di date: https://kotlinlang.org/docs/datetime.html
- Tutorial su formattazione delle date in Kotlin: https://www.baeldung.com/kotlin-format-date-time
- Domande frequenti sui formati di `DateTimeFormatter`: https://stackoverflow.com/questions/tagged/datetimeformatter?sort=faq