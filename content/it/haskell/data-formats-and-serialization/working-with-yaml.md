---
aliases:
- /it/haskell/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:33.628679-07:00
description: "YAML, acronimo di \"YAML Ain't Markup Language\", \xE8 uno standard\
  \ di serializzazione di dati amichevole per l'utente che pu\xF2 essere utilizzato\
  \ per tutti i\u2026"
lastmod: 2024-02-18 23:08:55.949654
model: gpt-4-0125-preview
summary: "YAML, acronimo di \"YAML Ain't Markup Language\", \xE8 uno standard di serializzazione\
  \ di dati amichevole per l'utente che pu\xF2 essere utilizzato per tutti i\u2026"
title: Lavorare con YAML
---

{{< edit_this_page >}}

## Cosa & Perché?

YAML, acronimo di "YAML Ain't Markup Language", è uno standard di serializzazione di dati amichevole per l'utente che può essere utilizzato per tutti i linguaggi di programmazione. Gli sviluppatori utilizzano spesso YAML nei file di configurazione e nello scambio di dati tra linguaggi a causa della sua leggibilità e della sua struttura semplice.

## Come fare:

Haskell non ha un supporto integrato per l'elaborazione di YAML, ma è possibile utilizzare librerie di terze parti come `yaml` e `aeson` per l'analisi e la generazione di dati YAML. Ecco come puoi iniziare:

### Leggere YAML
Prima, aggiungi il pacchetto `yaml` alle dipendenze del tuo progetto. Poi, puoi utilizzare il seguente esempio per analizzare un semplice documento YAML:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- Dati YAML di esempio
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- Definisci una struttura dati che corrisponda al documento YAML
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "Errore nell'analisi di YAML: " ++ show err
    Right person -> print person
```
Un esempio di output per il codice sopra potrebbe sembrare:
```
Person {name = "John Doe", age = 30}
```

### Scrivere YAML
Per generare YAML dalle strutture dati di Haskell, puoi usare le funzionalità di codifica del pacchetto `yaml` come mostrato di seguito:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- Utilizzando la struttura dati Person dall'esempio precedente

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
L'output di questo programma sarà una stringa formattata in YAML:
```
name: Jane Doe
age: 25
```

Questi esempi dovrebbero servire come punto di partenza per lavorare con YAML in Haskell. A seconda delle tue esigenze, potresti voler esplorare funzionalità e opzioni più avanzate offerte da queste librerie.
