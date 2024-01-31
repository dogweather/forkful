---
title:                "Lavorare con JSON"
date:                  2024-01-19
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON significa JavaScript Object Notation. Usiamo JSON perché è leggero, facile da leggere e scrivere per gli umani, e semplice da analizzare per i computer. È lo standard de facto per lo scambio di dati tra client e server su internet.

## How to:
In Haskell, lavoriamo con JSON tramite la libreria `aeson`. Installala con `cabal install aeson`.

```Haskell
-- Importa il modulo necessario
import Data.Aeson

-- Definisci un tipo di dato semplice
data Pizzeria = Pizzeria { nome :: String, pizzaPreferita :: String } deriving Show

-- Esempio di conversione da JSON
esempioJSON :: ByteString
esempioJSON = "{\"nome\": \"Da Gennaro\", \"pizzaPreferita\": \"Margherita\"}"

-- Decodifica JSON in un'istanza di Pizzeria
main :: IO ()
main = do
    let decodificaRisultato = decode esempioJSON :: Maybe Pizzeria
    case decodificaRisultato of
        Just pizzeria -> print pizzeria
        Nothing -> putStrLn "Non è stato possibile decodificare il JSON."
```

Output:
`Pizzeria {nome = "Da Gennaro", pizzaPreferita = "Margherita"}`

## Deep Dive
JSON è nato nel 2001. Alternativamente, potresti usare XML, che è più verboso, o protocol buffers, che sono più efficienti per la banda.

In Haskell, `aeson` fornisce `FromJSON` e `ToJSON` classi di tipo per decodificare e codificare. L'implementazione fa uso di combinators per costruire e destrutturare le strutture dati, rendendo il processo molto flessibile.

## See Also
- Documentazione ufficiale `aeson`: http://hackage.haskell.org/package/aeson
- Tutorial JSON in Haskell: https://artyom.me/aeson
- Confronto tra JSON, XML e Protocol Buffers: https://www.oreilly.com/ideas/answers-to-common-questions-about-json-and-xml
