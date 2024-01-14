---
title:    "Kotlin: Calcolare una data nel futuro o nel passato"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Calcolare le date nel futuro o nel passato può essere utile per pianificare eventi, tenere traccia di scadenze o semplicemente per capire quando un determinato giorno cadrebbe in una data determinata.

## Come fare
Per calcolare una data nel futuro o nel passato in Kotlin, è necessario utilizzare la classe `LocalDate` dalla libreria standard di Kotlin.
Ecco un esempio di codice che mostra come calcolare la data di domani:

```Kotlin
val today = LocalDate.now()
val tomorrow = today.plusDays(1)

println("Oggi è $today")
println("Domani sarà $tomorrow")
```

L'output di questo codice sarebbe:

```
Oggi è 2021-08-17
Domani sarà 2021-08-18
```

Per calcolare una data nel passato, è possibile utilizzare il metodo `minusDays()` invece di `plusDays()`.
È possibile anche specificare un numero diverso di anni, mesi o settimane da aggiungere o sottrarre utilizzando i rispettivi metodi `plusYears()`, `plusMonths()` e `plusWeeks()`.

## Approfondimento
La classe `LocalDate` utilizza il sistema ISO-8601 per rappresentare date e non tiene conto dei fusi orari.
È possibile utilizzare anche altre classi della libreria standard di Kotlin per gestire date e orari in modo più preciso, come ad esempio `LocalDateTime` o `ZonedDateTime`.

Un'altra opzione per calcolare date è utilizzare la libreria esterna [ThreeTen Extra](https://www.threeten.org/threeten-extra/), che fornisce funzionalità aggiuntive per la gestione di date e orari in Kotlin e altre lingue.

## Vedi anche
- [Documentazione di LocalDate in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Tutorial: Come manipolare date e orari in Kotlin](https://www.baeldung.com/kotlin/datetime-manipulation)
- [Documentazione di ThreeTen Extra](https://www.threeten.org/threeten-extra/)