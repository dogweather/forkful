---
title:                "Haskell: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che lavora con Haskell, probabilmente hai bisogno di ottenere la data attuale in qualche momento del tuo codice. Potresti voler registrare la data di creazione di un file, aggiornare un database o semplicemente stampare la data corrente per fini di debug. Indipendentemente dal tuo motivo, è importante sapere come ottenere la data corrente all'interno del tuo programma.

## Come fare

Fortunatamente, ottenere la data corrente in Haskell è un'operazione molto semplice grazie alla libreria standard Data.Time. Iniziamo importando la libreria all'inizio del nostro file:

```Haskell
import Data.Time
```

Ora, possiamo utilizzare la funzione getCurrentTime per ottenere la data e l'ora corrente:

```Haskell
currentDate <- getCurrentTime
```

La variabile `currentDate` conterrà un'istanza del tipo `UTCTime` che rappresenta la data e l'ora corrente in formato UTC. Possiamo anche convertire questa data in un formato più facile da leggere utilizzando la funzione `utcToLocalTime`:

```Haskell
localDate <- utcToLocalTime (hoursToTimeZone 2) currentDate
```

In questo esempio, stiamo convertendo la data in un formato di tempo locale aggiungendo due ore al tempo UTC. Possiamo quindi utilizzare la funzione `formatTime` per stampare la data in un formato specifico:

```Haskell
formattedDate <- formatTime defaultTimeLocale "%d/%m/%Y" localDate
```

Nel codice sopra, stiamo formattando la data nel formato "giorno/mese/anno". Se eseguiamo `print formattedDate`, vedremo la data corrente stampata nel formato desiderato.

## Approfondimento

Se vuoi approfondire la libreria Data.Time e imparare come ottenere la data e l'ora in formati diversi, puoi consultare la documentazione ufficiale di Haskell sul modulo Data.Time. Ci sono molte funzioni utili disponibili che possono aiutarti a manipolare e gestire la data all'interno del tuo programma.

### Vedi anche

- [Documentazione ufficiale di Haskell Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial su come ottenere la data corrente in Haskell](https://tech.channable.com/posts/2019-01-30-haskell-get-current-date.html)
- [Esempi di codice per gestire la data in Haskell](https://gist.github.com/raghavgautam/7dacc1f3ebcca24363e5fbd2f9451f3e)