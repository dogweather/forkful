---
title:    "Gleam: Confrontare due date"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Hai mai trovato difficile confrontare due date in un programma? Potresti aver notato che alcune lingue di programmazione hanno metodi integrati per confrontare date, ma Gleam non ne ha uno. Fortunatamente, Gleam ha una soluzione semplice che condivideremo in questo post.

## Come fare

Per confrontare due date in Gleam, dobbiamo prima convertirle in un formato supportato per confronti. Questo formato è rappresentato dal tipo Int, che rappresenta il numero di secondi trascorsi dalla mezzanotte del 1 gennaio 1970. Per fare ciò, possiamo utilizzare la funzione ```to_posix_seconds``` del modulo ```Date``` di Gleam.

Dopo aver convertito le date in Int, possiamo utilizzare gli operatori di confronto ```>```, ```<``` e ```=``` per confrontare le date. Di seguito un esempio di codice:

```
import gleam/datetime
import gleam/timezone

let oggi = Date.now
let domani = oggi + datetime.days(1)

let oggi_in_secondi = Date.to_posix_seconds(oggi, timezone.utc)

let domani_in_secondi = Date.to_posix_seconds(domani, timezone.utc)

if oggi_in_secondi > domani_in_secondi {
  // fa qualcosa
} elif oggi_in_secondi < domani_in_secondi {
  // fa qualcos'altro
} else {
  // le date sono uguali
}
```

Ecco un esempio di output:

```
Il giorno di oggi è maggiore di domani
```

## Approfondimento

Se vuoi eseguire confronti più specifici come ore, minuti o secondi, puoi utilizzare la funzione ```to_posix``` invece di ```to_posix_seconds```. Questa funzione restituirà un record contenente i valori dei singoli campi della data, che puoi quindi confrontare. Ad esempio:

```
import gleam/datetime
import gleam/timezone

let oggi = Date.now
let domani = oggi + datetime.days(1)

let oggi_in_secondi = Date.to_posix(oggi, timezone.utc)

let domani_in_secondi = Date.to_posix(domani, timezone.utc)

if oggi_in_secondi.hour > domani_in_secondi.hour {
  // fa qualcosa
} elif oggi_in_secondi.minute < domani_in_secondi.minute {
  // fa qualcos'altro
} else {
  // gli orari sono uguali
}
```

## Vedere anche

- Documentazione ufficiale di Gleam sul modulo Date: https://gleam.run/modules/gleam_datetime/latest/Date.html
- Tutorial di Gleam sul confronto di date: https://gleam.run/articles/working-with-dates.html#comparing-dates