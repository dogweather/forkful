---
title:                "Kotlin: Operare con json"
simple_title:         "Operare con json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che lavora con applicazioni mobili o sviluppa per il web, molto probabilmente hai a che fare con il formato JSON. Questo formato è molto comune per lo scambio di dati tra server e client e può essere utile anche per archiviare dati in modo leggibile. Per questo motivo, è importante per i programmatori avere una buona comprensione di come lavorare con JSON utilizzando il linguaggio di programmazione Kotlin.

## Come

Per cominciare a lavorare con JSON in Kotlin, è necessario importare la libreria kotlinx.serialization. Questa libreria fornisce le funzionalità necessarie per convertire oggetti Kotlin in formato JSON e viceversa. Ecco un esempio di come convertire un oggetto Kotlin in JSON utilizzando questa libreria:

```
Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*
val user = User("Mario", 25, "mario@example.com")
val json = Json.stringify(User.serializer(), user)
```
Nell'esempio sopra, abbiamo importato la libreria e creato un oggetto `user` di tipo `User` con alcuni valori. Successivamente, utilizziamo la funzione `Json.stringify` per convertire l'oggetto in formato JSON utilizzando il serializer di default per il tipo `User`.

Ecco l'output ottenuto:

```
{"name":"Mario","age":25,"email":"mario@example.com"}
```

È anche possibile convertire un oggetto JSON in un oggetto Kotlin. Ecco un esempio:

```
Kotlin
val jsonString = """{"name":"Mario","age":25,"email":"mario@example.com"}"""
val user = Json.parse(User.serializer(), jsonString)
``` 

Nell'esempio sopra, abbiamo creato una stringa che rappresenta un oggetto JSON e utilizzato la funzione `Json.parse` per convertirla in un oggetto di tipo `User` utilizzando lo stesso serializer del precedente esempio.

Oltre alla conversione degli oggetti, la libreria kotlinx.serialization offre anche funzioni per lavorare con stream di dati JSON e supporta la conversione di tipi di dati comuni come Int, Double e Boolean.

## Deep Dive

Se desideri approfondire ulteriormente il tuo sapere su JSON in Kotlin, puoi esplorare la documentazione ufficiale della libreria kotlinx.serialization. Lì troverai maggiori informazioni sugli schemi di serializzazione personalizzati, sui tipi di dati supportati e su come gestire casi più complessi come le liste e le classi con ereditarietà.

Non dimenticare di utilizzare debug e test per assicurarti che le tue conversioni da e verso JSON siano corrette. Un semplice errore può causare problemi difficili da individuare in un secondo momento.

## Vedi anche

- [Documentazione ufficiale kotlinx.serialization](https://github.com/Kotlin/kotlinx.serialization)
- [Gson: Libreria JSON per Kotlin](https://github.com/google/gson)
- [Corso gratuito su Kotlin su Udacity](https://www.udacity.com/course/kotlin-bootcamp-for-programmers--ud9011)