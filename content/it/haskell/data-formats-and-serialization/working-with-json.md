---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:53.911483-07:00
description: "Lavorare con JSON (JavaScript Object Notation) in Haskell comporta l'analisi\
  \ dei dati JSON in tipi Haskell e la conversione dei tipi Haskell di nuovo in\u2026"
lastmod: 2024-02-19 22:05:02.562372
model: gpt-4-0125-preview
summary: "Lavorare con JSON (JavaScript Object Notation) in Haskell comporta l'analisi\
  \ dei dati JSON in tipi Haskell e la conversione dei tipi Haskell di nuovo in\u2026"
title: Lavorare con JSON
---

{{< edit_this_page >}}

## Cos'è & Perché?
Lavorare con JSON (JavaScript Object Notation) in Haskell comporta l'analisi dei dati JSON in tipi Haskell e la conversione dei tipi Haskell di nuovo in JSON. I programmatori fanno ciò per consentire alle loro applicazioni Haskell di scambiare dati con servizi web o API senza problemi, una pratica comune nello sviluppo software moderno per lo scambio di dati cross-platform.

## Come fare:
Haskell non ha un supporto integrato per JSON come JavaScript, ma con l'aiuto di librerie di terze parti come **Aeson**, la gestione del JSON diventa semplice. Aeson fornisce funzioni sia ad alto livello che a basso livello per la codifica (conversione dei valori Haskell in JSON) e la decodifica (analisi del JSON in valori Haskell).

### Installare Aeson
Prima di tutto, aggiungi Aeson alle dipendenze del tuo progetto aggiornando il tuo file `.cabal` o utilizzando direttamente Stack o Cabal:

```shell
cabal update && cabal install aeson
```
oppure, se stai utilizzando Stack:
```shell
stack install aeson
```

### Analisi di JSON
Iniziamo con un esempio di base di decodifica di dati JSON in un tipo Haskell. Supponiamo di avere il seguente JSON che rappresenta una persona:

```json
{
  "name": "John Doe",
  "age": 30
}
```

Prima, definisci un corrispondente tipo di dato Haskell e rendilo un'istanza di `FromJSON`:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- Funzione per decodificare JSON da un file
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Utilizzo:
Assumendo che `person.json` contenga i dati JSON mostrati sopra, esegui:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Output di esempio:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Codifica di Valori Haskell come JSON
Per convertire un valore Haskell di nuovo in JSON, è necessario rendere il tuo tipo un'istanza di `ToJSON` e quindi utilizzare `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Assumendo il tipo Person di prima

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Output di esempio:
```json
{"name":"Jane Doe","age":32}
```

Questi esempi dimostrano le basi del lavoro con JSON in Haskell usando Aeson. Ricorda, Aeson offre molto di più, incluse regole di parsing personalizzate, lavoro con JSON annidati complessi e molto altro, adatto a varie esigenze e scenari.
