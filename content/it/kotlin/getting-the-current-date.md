---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?
Ottenere la data corrente in un programma consente di operare su timestamp, log events, o semplicemente mostrare la data corrente all'utente. Questo può essere molto utile, ad esempio, per segnare l'ora di arrivo di un messaggio in un'app di chat.

## Come Fare:
La libreria standard di Kotlin fornisce già una serie di funzionalità per questa operazione. Qui sotto un esempio pratico:

```Kotlin
import java.time.LocalDate

fun main() {
    val oggi = LocalDate.now()
    println("La data di oggi è: $oggi")
}
```

Nel momento in cui eseguirai questo codice, otterrai un output del tipo:

```bash
La data di oggi è: 2023-02-10
```

## Approfondimento
A livello storico, prima di Java 8 e Kotlin, la gestione delle date era piuttosto maldestra e propensa ad errori. Con l'introduzione delle nuove API per la gestione del tempo in Java 8, che sono pienamente supportate da Kotlin, si ha un maggiore controllo e facilità d'uso.

Una alternativa all'uso di `LocalDate.now()` potrebbe essere `java.util.Date` di Java, che però come già segnalato tende a essere più problematico.

Il metodo `now()` di `LocalDate` funziona ottenendo la data corrente dal sistema predefinito del clock. Restituisce quindi una data senza l'informazione del tempo, contrariamente a `LocalDateTime.now()`, che restituisce anche l'ora corrente.

## Altro da Vedere
Se vuoi un approfondimento sulle classi di date e tempo di Kotlin, il link qui sotto può esserti utile:
- [Tutorial di Baeldung sulle Date API di Kotlin](https://www.baeldung.com/kotlin/dates) 

Se invece sei interessato a capire come ottenere la data corrente in altri linguaggi di programmazione, dai un'occhiata ai link che seguono:
- [Ottenere la data corrente in Java](https://www.w3schools.com/java/java_date.asp)
- [Ottenere la data corrente in Python](https://www.w3schools.com/python/python_datetime.asp)