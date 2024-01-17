---
title:                "Lavorare con yaml"
html_title:           "Haskell: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Lavorare con YAML è un modo per gestire e organizzare i dati in un formato più facile da leggere e scrivere per i programmatori. Spesso viene utilizzato per configurare le impostazioni di un'applicazione o per gestire una grande quantità di dati. 

## Come Fare:

Un esempio di come utilizzare YAML in Haskell potrebbe essere il seguente:

```
import Data.Yaml (decodeFileThrow)

main :: IO ()
main = do
  -- Esempio di lettura dei dati da un file YAML
  utenti <- decodeFileThrow "utenti.yaml" :: IO [Utente]
  -- Esempio di stampa dei dati letti
  mapM_ (putStrLn . show) utenti
```

Questo codice utilizza la libreria `Data.Yaml` per decodificare un file YAML e convertirlo in una struttura dati di tipo `Utente`. Successivamente, viene stampato l'elenco degli utenti letti dal file. 

## Approfondimento:

YAML è stato sviluppato nel 2001 da Clark Evans ed è diventato uno standard universale per la gestione dei dati strutturati. Sebbene sia utilizzato principalmente nei linguaggi di programmazione, può essere anche letto e scritto da umani in modo leggibile. 

Alcune alternative a YAML sono JSON e XML, ma YAML è spesso preferito per la sua sintassi più semplice e intuitiva. Anche se la versione attuale di YAML è la 1.2, alcuni strumenti supportano ancora la versione precedente 1.1. 

Per implementare l'utilizzo di YAML in Haskell, è necessario importare la libreria `Data.Yaml` e utilizzare le sue funzioni per leggere e scrivere i dati in formato YAML. Inoltre, è possibile utilizzare la libreria `Data.Aeson` per convertire i dati YAML in altri formati come JSON. 

## Vedi Anche:

- [Documentazione ufficiale di YAML](https://yaml.org)
- [Tutorial su YAML in Haskell](https://www.stackbuilders.com/tutorials/haskell/yaml/) 
- [Progetto GitHub della libreria `Data.Yaml`](https://github.com/snoyberg/yaml)