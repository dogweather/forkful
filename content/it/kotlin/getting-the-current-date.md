---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:15:27.231738-07:00
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Ottenere la data corrente in programmazione significa semplicemente acquisire il momento attuale secondo il calendario. I programmatori lo fanno per registrare eventi, confrontare date o semplicemente visualizzare informazioni temporali agli utenti.

## Come fare:
In Kotlin, l'ottenimento della data corrente è diretto grazie alle librerie di date e ore.

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val oggi = LocalDate.now()
    val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
    val dataFormattata = oggi.format(formatter)

    println("Data corrente: $dataFormattata") // Output esempio: Data corrente: 12/03/2023
}
```

## Approfondimento:
In passato, i programmatori utilizzavano `java.util.Date` e `java.util.Calendar` per ottenere la data, ma queste vecchie classi avevano diversi difetti e problemi di design. La libreria `java.time`, introdotta in Java 8 e quindi disponibile in Kotlin, ha risolto molti di questi problemi, offrendo una gestione ottimale del tempo. Esistono alternative a `java.time`, come Joda-Time, ma da quando `java.time` è diventata la scelta standard, l'uso di Joda-Time è calato molto.

La gestione della data corrente implica spesso più che semplicemente ottenere il giorno corrente; bisogna considerare il fuso orario e la localizzazione. Inoltre, ci sono diverse sfumature quando si lavora con le date, come l'immutabilità degli oggetti e la gestione di date invalide o di casi limite come l'inserimento dell'orario legale.

## Vedi Anche:
- Java `DateTimeFormatter`: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- `java.time` package overview: [oracle docs](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Comparazione tra Joda-Time e `java.time`: [Blog post](https://blog.joda.org/2014/11/converting-from-joda-time-to-javatime.html)
