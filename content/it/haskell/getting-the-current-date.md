---
title:                "Ottenere la data corrente"
html_title:           "Haskell: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Se stai sviluppando un programma o un'applicazione che richiede conoscenza sulla data corrente, come ad esempio un calendario o un promemoria, avrai bisogno di codice che prenda la data corrente dal sistema. In Haskell, esistono alcune librerie e funzioni predefinite che rendono questa operazione molto semplice e rapida.

## Come fare

Per ottenere la data corrente in Haskell, è necessario importare il modulo `Data.Time` che contiene le funzioni per manipolare il tempo. Una volta importato il modulo, puoi utilizzare la funzione `getCurrentTime` che restituisce un oggetto di tipo `IO UTCTime`, ovvero un'azione di input/output che restituisce l'ora corrente in formato UTC.

```Haskell
import Data.Time -- Importa il modulo "Data.Time"

main = do
    currentTime <- getCurrentTime -- Salva l'ora corrente nella variabile "currentTime"
    print currentTime -- Stampa l'ora corrente
```

L'output di questo codice sarà qualcosa del genere: `2021-11-23 10:30:00 UTC`. Tieni presente che l'ora restituita è nel formato UTC, quindi potrebbe non corrispondere all'ora locale del tuo computer.

È anche possibile ottenere l'ora corrente in un formato più leggibile utilizzando la funzione `getCurrentTime` insieme alla funzione `formatTime` che accetta un pattern di formattazione come argomento. Ad esempio, se vogliamo stampare l'ora corrente nel formato "gg/mm/aaaa hh:mm:ss", il codice sarebbe il seguente:

```Haskell
import Data.Time.Format -- Importa il modulo "Data.Time.Format"

main = do
    currentTime <- getCurrentTime -- Salva l'ora corrente nella variabile "currentTime"
    let formattedTime = formatTime defaultTimeLocale "%d/%m/%Y %H:%M:%S" currentTime -- Applica il formato desiderato all'ora corrente
    print formattedTime -- Stampa l'ora corrente nel formato specificato
```

L'output sarà qualcosa del genere: `24/11/2021 10:30:00`.

## Approfondimento

La funzione `getCurrentTime` utilizza l'orologio di sistema per ottenere l'ora corrente, che potrebbe non essere sempre affidabile. Per garantire la precisione dell'ora corrente, è consigliabile utilizzare la libreria `TZ` che utilizza un server di tempo NTP per ottenere l'ora corrente in formato UTC.

Per utilizzare la libreria TZ, è necessario installarla utilizzando `cabal` e quindi importarla nel tuo codice. Ecco un esempio di come utilizzare la libreria per ottenere l'ora corrente in UTC:

```Haskell
import Data.Time.TZ -- Importa il modulo "Data.Time.TZ"

main = do
    currentTime <- getCurrentTimeTZ "pool.ntp.org" -- Specifica il server NTP da utilizzare
    print currentTime -- Stampa l'ora corrente in formato UTC
```

## Vedi anche

- [Documentazione ufficiale su Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Documentazione ufficiale su TZ](https://hackage.haskell.org/package/tz/docs/Data-Time-TZ.html)