---
title:                "Haskell: Leggere gli argomenti da linea di comando"
simple_title:         "Leggere gli argomenti da linea di comando"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler leggere gli argomenti della riga di comando in Haskell. Ad esempio, potresti voler passare input personalizzati al tuo programma o gestire opzioni diverse in base alle situazioni.

## Come fare

Per prima cosa, importa il modulo System.Environment per ottenere accesso alle funzioni per leggere gli argomenti della riga di comando.

```Haskell
import System.Environment

main = do
    args <- getArgs
    -- Alcune elaborazioni con gli argomenti qui..
```

In questo esempio, la funzione `getArgs` restituisce una lista di stringhe contenente tutti gli argomenti della riga di comando passati al programma. Successivamente, puoi utilizzare queste stringhe per eseguire le elaborazioni necessarie all'interno della tua applicazione.

Inoltre, è possibile utilizzare anche la funzione `getProgName` per ottenere il nome del programma in esecuzione.

```Haskell
import System.Environment

main = do
    name <- getProgName
    putStrLn ("Il programma in esecuzione è " ++ name)
```

## Approfondimento

In Haskell, è possibile utilizzare anche la libreria `optparse-applicative` per leggere e gestire in modo più avanzato gli argomenti della riga di comando. Questa libreria permette di definire le opzioni disponibili, analizzare gli argomenti passati e gestire gli errori. Ecco un esempio di codice che utilizza `optparse-applicative`:

```Haskell
import Options.Applicative

-- Definizione delle opzioni
data Options = Options
    { nome :: String
    , eta :: Int
    , lingue :: [String]
    } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption
        ( long "name"
        <> metavar "NOME"
        <> help "Il nome dell'utente" )
    <*> option auto
        ( long "age"
        <> metavar "ETA"
        <> help "L'età dell'utente" )
    <*> many (strOption
        ( long "language"
        <> short 'l'
        <> metavar "LINGUA"
        <> help "Una delle lingue conosciute dall'utente" ))

main :: IO ()
main = do
    opzioni <- execParser (info optionsParser
                         ( fullDesc
                         <> progDesc "Un semplice programma di benvenuto"
                         <> header "Benvenuto!" ))
    putStrLn ("Ciao " ++ nome opzioni ++ "! Sei " ++ show (eta opzioni) ++ " anni e parli " ++ show (lingue opzioni))
```

Questo esempio definisce un tipo di dati `Options` con tre campi per il nome, l'età e le lingue dell'utente. Successivamente, tramite la libreria `optparse-applicative`, si definiscono le opzioni disponibili, come `--name` o `--age`. Infine, all'interno della funzione `main`, viene utilizzata la funzione `execParser` per analizzare gli argomenti passati e restituire il valore `Options` corrispondente.

## Vedi anche

- Documentazione del modulo System.Environment su Hackage: [https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- Documentazione della libreria `optparse-applicative` su Hackage: [https://hackage.haskell.org/package/optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)