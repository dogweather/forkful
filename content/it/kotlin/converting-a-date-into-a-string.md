---
title:    "Kotlin: Convertire una data in una stringa"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con le date in un programma Kotlin, è necessario convertire una data in una stringa. Questo può essere utile per mostrare la data in un formato specifico o per utilizzarla in un database. In questo articolo, vedremo come convertire una data in una stringa utilizzando Kotlin.

## Come

Per convertire una data in una stringa, si può utilizzare il metodo `format()` della classe `DateTimeFormatter` di Kotlin. Questo metodo accetta come parametro un oggetto di tipo `LocalDate` o `LocalDateTime`, che rappresenta rispettivamente una data o una data e un'ora.

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

// Creazione di un oggetto LocalDate con la data desiderata
val data: LocalDate = LocalDate.of(2021, 5, 18)

// Creazione di un oggetto DateTimeFormatter per specificare il formato della data
val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")

// Utilizzo del metodo format() per convertire la data in una stringa nel formato specificato
val dataString: String = formatter.format(data)

println(dataString) // Output: 18/05/2021
```

Si può anche utilizzare il metodo `parse()` della classe `DateTimeFormatter` per convertire una stringa in un oggetto `LocalDate` o `LocalDateTime`.

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

// Creazione di un oggetto DateTimeFormatter per specificare il formato della data
val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")

// Creazione di una stringa con una data nel formato specificato
val dataString: String = "28/09/2021"

// Utilizzo del metodo parse() per convertire la stringa in un oggetto LocalDate
val data: LocalDate = LocalDate.parse(dataString, formatter)

println(data) // Output: 2021-09-28
```

## Deep Dive

Oltre al metodo `format()` e `parse()`, la classe `DateTimeFormatter` offre molti altri metodi utili per la manipolazione delle date. Alcuni di questi metodi sono:

- `getAvailableDateTimeFormats()`: restituisce una lista di tutti i formati disponibili per la formattazione delle date.
- `getLocalizedDateTimePattern()`: restituisce il formato della data localizzato in base alla lingua e alle impostazioni regionali del sistema operativo.
- `formatTo()` e `parseToTemporalAccessor()`: metodi utili per la formattazione e la conversione di una data in oggetti di tipo `TemporalAccessor`.

Oltre alla classe `DateTimeFormatter`, Kotlin offre altre classi come `SimpleDateTimeFormatter` e `DateTimeFormatterBuilder` per la gestione delle date.

## Vedi anche

- [Documentazione ufficiale di Kotlin sulla manipolazione delle date](https://kotlinlang.org/docs/datetime.html)
- [Tutorial su come utilizzare le classi di manipolazione delle date in Kotlin](https://www.geeksforgeeks.org/date-and-time-manipulation-using-kotlin/) 
- [Esempi pratici di conversione di date in stringhe con Kotlin](https://levelup.gitconnected.com/dates-and-time-in-kotlin-a0ecb86c6e3b)