---
title:                "Lavorare con json"
html_title:           "Haskell: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-json.md"
---

{{< edit_this_page >}}

# Perché lavorare con JSON in Haskell

Se sei appassionato di sviluppo software, sicuramente avrai sentito parlare di JSON (JavaScript Object Notation). Questo formato di dati molto utilizzato nella comunicazione tra applicazioni è diventato fondamentale nel mondo dello sviluppo web e mobile. L'utilizzo di JSON è molto diffuso anche in Haskell, grazie alla sua flessibilità e alla presenza di librerie specifiche per la manipolazione dei dati in questo formato.

# Come lavorare con JSON in Haskell

Per iniziare a utilizzare JSON in Haskell, dobbiamo importare il modulo `Data.Aeson` all'interno del nostro codice. Questo modulo ci permette di serializzare e deserializzare i dati in formato JSON. Ecco un semplice esempio:

```Haskell
import Data.Aeson

-- Definiamo una struttura dati in Haskell
data Persona = Persona { nome :: String, cognome :: String, eta :: Int }
    deriving (Show, Generic)

-- Utilizziamo la funzione `encode` per serializzare i nostri dati in formato JSON
toJSON (Persona "Mario" "Rossi" 35)
-- Output: {"nome":"Mario","cognome":"Rossi","eta":35}

-- Utilizziamo la funzione `decode` per deserializzare un dato in formato JSON in una struttura dati Haskell
fromJust (decode "{\"nome\":\"Laura\",\"cognome\":\"Bianchi\",\"eta\":25}" :: Maybe Persona)
-- Output: Just (Persona {nome = "Laura", cognome = "Bianchi", eta = 25})
```

Come puoi vedere, utilizzare JSON in Haskell è molto semplice grazie alla presenza di funzioni apposite.

# Approfondimento sul lavoro con JSON in Haskell

Per manipolare i dati in formato JSON in modo più avanzato, possiamo utilizzare la libreria `aeson` che ci offre più opzioni e strumenti per lavorare con questo formato. Ad esempio, possiamo definire una funzione per effettuare una ricerca all'interno di una lista di oggetti JSON in base a determinati criteri. Inoltre, possiamo utilizzare la funzione `withObject` per accedere ai campi di un oggetto JSON in modo più strutturato.

# Vedi anche

- [Documentazione ufficiale di `Data.Aeson`](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html)
- [Guida completa all'utilizzo di JSON in Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/json-parsing-with-aeson)
- [Esempi pratici di utilizzo di JSON in Haskell](https://www.stackage.org/nightly-2017-12-12/package/aeson-compat-0.3.6.1)