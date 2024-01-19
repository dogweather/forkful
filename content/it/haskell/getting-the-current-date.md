---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cos'è e Perchè?

Ottenere la data corrente significa recuperare la data e l'ora esatte in cui il tuo programma sta girando. Questo può essere utile per molte cose, come la registrazione di quando un evento specifico si è verificato nel sistema o la tracciatura di date di scadenza.

## Come Fare:

Per ottenere la data corrente in Haskell, useremo la libreria `Data.Time.Clock`. Ecco un esempio:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

main = do
    tempo_corrente <- getCurrentTime
    let giorno_corrente = utctDay tempo_corrente
    print giorno_corrente
```

Eseguendo questo codice, otterrai un output simile a questo:

```Haskell
>>> main
2022-02-22
```

## Approfondimento:

Historicamente, ottenere la data corrente è un problema comune in programmazione, indipendentemente dal linguaggio utilizzato. In Haskell, la libreria `Data.Time.Clock` è molto comune per ottenere la data corrente. Tuttavia ci sono alternative, come `System.Time` che fornisce la funzione `getClockTime`. 

```Haskell
import System.Time

main = do
    tempo_corrente <- getClockTime
    print tempo_corrente
```

Però, `System.Time` è deprecato nella versione attuale di Haskell. Preferiamo usare `Data.Time.Clock` che ha un'implementazione più robusta e flessibile.

## Da Vedere Anche:

Per ottenere ulteriori informazioni, consulta le seguenti risorse:

1. Documentazione ufficiale per [`Data.Time.Clock`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html)
2. Una guida completa alla [programmazione di data e ora in Haskell](http://two-wrongs.com/haskell-time-library-tutorial)
3. Domande e risposte correlate su [Stack Overflow](https://stackoverflow.com/questions/tagged/haskell+date)