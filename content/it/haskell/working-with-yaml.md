---
title:                "Haskell: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Sebbene YAML sia un formato di dati relativamente nuovo rispetto ad altre opzioni più consolidate, come ad esempio JSON o XML, sta guadagnando sempre più consensi tra i programmatori grazie alla sua semplicità e flessibilità. Con YAML, è possibile rappresentare facilmente dati in formato leggibile sia dalle persone che dalle macchine, rendendolo ideale per una vasta gamma di applicazioni.

## Come fare

In Haskell, esistono diversi modi per lavorare con YAML. Uno dei più semplici è utilizzare il pacchetto `yaml`, disponibile su Hackage. Per iniziare, sarà necessario importare il modulo `Data.Yaml` nel tuo file sorgente. Poi, puoi utilizzare la funzione `decodeFileEither` per leggere i dati da un file YAML e convertirli in una struttura di dati Haskell:

```Haskell
import Data.Yaml

main :: IO ()
main = do
  result <- decodeFileEither "dati.yaml" :: IO (Either ParseException YourDataType)
  case result of
    Left err -> putStrLn $ "Errore: " ++ show err
    Right dati -> putStrLn $ "Dati caricati con successo: " ++ show dati
```

Se il tuo file YAML non corrisponde alla tua struttura di dati Haskell, verrà restituito un errore di tipo `ParseException`. In caso contrario, il tuo file verrà convertito nella tua struttura di dati e potrai utilizzarlo nel tuo programma.

## Approfondimento

Oltre alla semplice lettura di file YAML, è possibile anche utilizzarlo per creare e scrivere file YAML utilizzando la funzione `encodeFile`. Inoltre, il pacchetto `yaml` offre anche funzionalità avanzate, come la possibilità di specificare schemi di validazione per assicurarsi che i dati letti da un file YAML siano corretti.

Ci sono anche altri pacchetti disponibili su Hackage per lavorare con YAML in Haskell, come ad esempio `aeson-yaml` che offre la possibilità di convertire dati tra YAML e JSON.

## Vedi anche

- [Documentazione del pacchetto yaml su Hackage](https://hackage.haskell.org/package/yaml)
- [Pacchetto aeson-yaml su Hackage](https://hackage.haskell.org/package/aeson-yaml)
- [Approfondimento sul formato YAML](https://yaml.org/spec/1.2/spec.html)