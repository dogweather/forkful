---
title:                "Haskell: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Negli ultimi anni, la gestione dei dati in formato JSON è diventata sempre più importante nel mondo dello sviluppo software. JSON (JavaScript Object Notation) è un formato dati leggibile dalle macchine e molto utilizzato per lo scambio di informazioni tra diverse applicazioni. In questo articolo, scopriremo come lavorare con JSON utilizzando il linguaggio di programmazione Haskell.

## Come Fare

Per prima cosa, dobbiamo importare il modulo `Data.Aeson` che ci permette di gestire facilmente i dati JSON in Haskell. Successivamente, definiremo il nostro primo oggetto JSON utilizzando la funzione `object` che prende come argomento una lista di coppie chiave-valore.

```Haskell
import Data.Aeson

-- Definizione di un oggetto JSON
myObject = object [
    "nome" .= "Maria",
    "cognome" .= "Rossi",
    "età" .= 30
    ]
```

Ora possiamo trasformare il nostro oggetto JSON in una stringa utilizzando la funzione `encode` e stamparla a schermo grazie alla funzione `putStrLn`.

```Haskell
main = do
    let jsonString = encode myObject
    putStrLn jsonString
```

L'output sarà il seguente:

```
{"nome": "Maria", "cognome": "Rossi", "età": 30}
```

Per leggere un file JSON e trasformarlo in un oggetto Haskell, dobbiamo utilizzare la funzione `decodeFileStrict` che prende come argomento il percorso del file e restituisce un `Maybe Value`. Utilizzando la funzione `fromJust` possiamo estrarre il valore dall'`Maybe` e lavorare con esso.

```Haskell
import Data.Maybe (fromJust)

-- Leggere un file JSON e trasformarlo in un oggetto Haskell
main = do
    jsonContent <- decodeFileStrict "persone.json"
    let persone = fromJust jsonContent
    putStrLn ("Nome: " ++ (persone ! "nome"))
```

## Approfondimento

In aggiunta alle funzioni già viste, il modulo `Data.Aeson` offre molte altre funzioni utili per lavorare con JSON. Ad esempio, la funzione `encodePretty` permette di indentare la stringa JSON in modo da renderla più leggibile. Inoltre, come visto nell'esempio precedente, possiamo accedere agli elementi di un oggetto utilizzando l'operatore `!` e specificando il nome del campo desiderato.

Per ulteriori informazioni e opzioni sul modulo `Data.Aeson`, si consiglia di consultare la documentazione ufficiale.

## Vedi Anche
- [Haskell e JSON: una guida introduttiva](https://www.functionalife.com/it/haskell-json-introduction/)
- [JSON in Haskell](https://hackage.haskell.org/package/aeson)
- [Guida completa alla gestione di dati JSON in Haskell](https://medium.com/@awakesecurity/manipulating-json-data-in-haskell-d7e7b911bf52)