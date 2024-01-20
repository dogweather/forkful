---
title:                "Lavorare con i file csv"
html_title:           "Haskell: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Lavorare con CSV, ovvero Comma Separated Values, è un modo comune per gestire e manipolare grandi quantità di dati tabellari. Questo formato, che utilizza una virgola come separatore tra i valori dei dati, è ampiamente utilizzato nel mondo della programmazione per importare ed esportare dati da database, fogli di calcolo e altre fonti.

I programmatori utilizzano CSV per semplificare il processo di gestione dei dati tabellari e per facilitare lo scambio di informazioni tra sistemi diversi.

## Come fare:

Per lavorare con CSV in Haskell, avrai bisogno del pacchetto ```Data.Csv```. Dopo averlo importato nel tuo file di codice, puoi utilizzare le sue funzioni per leggere e scrivere dati CSV.

### Leggere da un file CSV

Per leggere i dati da un file CSV, utilizza la funzione ```decode```, specificando il tuo tipo di dati desiderato e il file di input come parametri.

```Haskell
import Data.Csv

data Persona = Persona 
    { nome :: String
    , cognome :: String
    , eta :: Int
    } deriving (Show, Generic)

instance FromRecord Persona

leggiCSV :: String -> IO [Persona]
leggiCSV fileName = do
    content <- readFile fileName
    case decode NoHeader content :: Either String (Vector Persona) of
        Left err -> error err
        Right v -> return $ toList v
```

Nell'esempio sopra, creiamo un nuovo tipo di dati ```Persona``` con campi ```nome```, ```cognome``` ed ```eta```, e lo rendiamo un'istanza di ```FromRecord``` per indicare come i dati devono essere convertiti dal CSV.

Utilizziamo quindi la funzione ```leggiCSV``` per leggere i dati dal file specificato e otteniamo una lista di valori di tipo ```Persona```.

### Scrivere su un file CSV

Per scrivere i dati in un file CSV, utilizza la funzione ```encodeDefaultOrderedByName```, specificando il nome delle colonne e i dati da scrivere come parametri.

```Haskell
import qualified Data.ByteString.Lazy as BL
import Data.Csv

data Prodotto = Prodotto
    { nome :: String
    , prezzo :: Float
    , quantità :: Int
    } deriving (Show, Generic)

instance ToNamedRecord Prodotto where
    toNamedRecord prodotto = namedRecord
        [ "Nome" .= nome prodotto
        , "Prezzo" .= prezzo prodotto
        , "Quantità" .= quantità prodotto
        ]

prodotti = [ Prodotto "Penna" 1.50 100
           , Prodotto "Quaderno" 4.50 50
           , Prodotto "Matita" 0.75 200
           ]

scriviCSV :: String -> IO ()
scriviCSV fileName = BL.writeFile fileName (encodeDefaultOrderedByName prodotti)
```

Nell'esempio sopra, creiamo un nuovo tipo di dati ```Prodotto``` con campi ```nome```, ```prezzo``` e ```quantità```, e lo rendiamo un'istanza di ```ToNamedRecord``` per indicare come i dati devono essere convertiti in formato CSV.

Utilizziamo quindi la funzione ```scriviCSV``` per scrivere i dati nella lista di prodotti nel file specificato.

## Approfondimento:

### Contesto storico:

CSV è stato originariamente sviluppato nel 1972 da un programmatore della IBM, ma è diventato popolare solo negli anni '80 con l'avvento dei fogli di calcolo e dei database relazionali. Oggi è uno dei formati più comunemente utilizzati per l'importazione ed esportazione di dati.

### Altre alternative:

Sebbene CSV sia un formato comune e popolare, ci sono altre alternative disponibili per lavorare con dati tabellari in Haskell. Ad esempio, ci sono pacchetti che supportano formati più strutturati come JSON o XML.

### Dettagli di implementazione:

Il pacchetto ```Data.Csv``` utilizza il tipo di dati ```Record``` per rappresentare una riga di dati CSV, e fornisce funzioni per convertire tra questo tipo e altri tipi di dati personalizzati.

## Vedi anche:

- [Hackage: Data.Csv](http://hackage.haskell.org/package/cassava)