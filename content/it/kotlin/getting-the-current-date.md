---
title:                "Kotlin: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ottenere la data corrente è un'attività comune in programmazione, in particolare quando si lavora con applicazioni che richiedono l'aggiornamento costante dei dati. Sapere come ottenere la data corrente in Kotlin può semplificare il processo e aumentare l'efficienza nella gestione delle informazioni temporali.

## Come fare

```Kotlin
//crea un oggetto della classe LocalDateTime
val now = LocalDateTime.now()

//use un formato specifico per visualizzare la data e l'orario
val formattedDate = now.format(DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"))

//stampa il risultato
println(formattedDate)
```

L'output di questo codice sarà "16/10/2021 21:30:15", ovvero la data e l'orario correnti. È possibile personalizzare il formato utilizzando diverse opzioni offerte da DateTimeFormatter come la visualizzazione in formato 12 o 24 ore, la visualizzazione dell'orario in fuso orario, ecc.

## Approfondimento

Oltre all'utilizzo di LocalDateTime come mostrato nell'esempio precedente, Kotlin offre anche altre opzioni per ottenere la data corrente:

- Per ottenere solo la data corrente senza l'orario, è possibile utilizzare il metodo LocalDate.now().
- Per ottenere solo l'orario corrente senza la data, è possibile utilizzare il metodo LocalTime.now().
- Se si desidera ottenere la data corrente in un fuso orario specifico, è possibile utilizzare il metodo ZonedDateTime.now(ZoneId.of("nome del fuso orario")).
- È anche possibile impostare manualmente la data e l'orario utilizzando il metodo LocalDateTime.of(anno, mese, giorno, ora, minuti, secondi), ad esempio LocalDateTime.of(2021,10,16,21,30,15).

Queste sono solo alcune delle opzioni disponibili per ottenere la data corrente in Kotlin. Utilizzando la giusta combinazione di metodi e formattazione, è possibile adattare l'output ai propri bisogni specifici.

## Vedi anche

- Documentazione ufficiale di Java sulla classe LocalDateTime: <https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html>
- Documentazione ufficiale di Kotlin sulla classe LocalDateTime: <https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date-time/index.html>
- Tutorial su come utilizzare la classe LocalDateTime in Kotlin: <https://www.baeldung.com/kotlin/local-date-time>