---
title:                "Trasformare una data in una stringa"
html_title:           "Kotlin: Trasformare una data in una stringa"
simple_title:         "Trasformare una data in una stringa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Convertire una data in una stringa in Kotlin

## Cos'è e perché?

Convertire una data in una stringa significa rappresentare una data in un formato leggibile per gli esseri umani. Questo è utile quando si vuole mostrare la data su un'interfaccia utente o salvarla in un database. I programmatori fanno questo per migliorare l'usabilità del loro codice e rendere più semplice la gestione delle date.

## Come fare:

#### Esempio 1:
```
val date = SimpleDateFormat("dd/MM/yyyy").format(Date())
println(date)
```
Output: 20/07/2020

In questo esempio, abbiamo utilizzato la classe SimpleDateFormat per convertire la data corrente in una stringa in formato "giorno/mese/anno" e poi l'abbiamo stampata a video.

#### Esempio 2:
```
val sdf = SimpleDateFormat("dd MMMM yyyy", Locale.ITALIAN)
val date = sdf.parse("25 luglio 2020")
println(date)
```
Output: Sat Jul 25 00:00:00 GMT 2020

Qui abbiamo utilizzato la classe SimpleDateFormat anche per il viceversa, ovvero convertire una stringa in una data. Nota che abbiamo specificato anche la lingua locale per ottenere il nome del mese in italiano.

## Approfondimento:

### Contesto storico:
La necessità di convertire le date in stringhe e viceversa è nata con l'avvento dei computer e la gestione delle informazioni digitali. Prima di questo, le date venivano solitamente scritte in un formato leggibile per gli esseri umani, ad esempio "25 luglio 2020". Con l'avvento dei computer, è stato necessario standardizzare il formato delle date per un facile utilizzo nei sistemi informatici.

### Alternative:
Oltre alla classe SimpleDateFormat, Kotlin offre anche altre opzioni per la conversione delle date, come la classe DateTimeFormatter di Java 8 e la libreria Joda-Time.

### Dettagli di implementazione:
La classe SimpleDateFormat, come suggerisce il nome, offre una semplice e flessibile implementazione per la formattazione delle date. Utilizza come base il formato della classe java.text.DateFormat e supporta molti simboli per la formattazione personalizzata della data.

## Vedi anche:
- [Classe SimpleDateFormat di Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/)
- [Libreria Joda-Time](https://www.joda.org/joda-time/)
- [Classe DateTimeFormatter di Java 8](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)