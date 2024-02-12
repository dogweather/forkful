---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-02-03T19:16:56.842768-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo delle espressioni regolari"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è e perché?
Le espressioni regolari nella programmazione sono sequenze di caratteri che definiscono un modello di ricerca, tipicamente impiegate per la ricerca e la manipolazione di stringhe. I programmatori Haskell utilizzano le espressioni regolari per compiti che vanno dalla semplice corrispondenza di stringhe all'elaborazione complessa di testi, sfruttando la loro efficienza e versatilità nel trattare i dati testuali.

## Come fare:
In Haskell, le funzionalità regex non fanno parte della libreria standard, rendendo necessario l'uso di pacchetti di terze parti come `regex-base` insieme a un backend compatibile come `regex-posix` (per il supporto regex POSIX), `regex-pcre` (per regex compatibili con Perl), ecc. Ecco come puoi utilizzare questi pacchetti per lavorare con le espressioni regolari.

Prima di tutto, assicurati di avere i pacchetti installati aggiungendo `regex-posix` o `regex-pcre` al file `.cabal` del tuo progetto o installandoli direttamente tramite cabal:

```bash
cabal install regex-posix
```
oppure
```bash
cabal install regex-pcre
```

### Utilizzando `regex-posix`:

```haskell
import Text.Regex.Posix ((=~))

-- Verifica se una stringa corrisponde a un modello
isMatch :: String -> String -> Bool
isMatch testo modello = testo =~ modello :: Bool

-- Trova la prima corrispondenza
findFirst :: String -> String -> String
findFirst testo modello = testo =~ modello :: String

main :: IO ()
main = do
    print $ isMatch "ciao mondo" "mo"
    -- Output: True
    print $ findFirst "buongiorno, buonanotte" "buon"
    -- Output: "buon"
```

### Utilizzando `regex-pcre`:

```haskell
import Text.Regex.PCRE ((=~))

-- Trova tutte le corrispondenze
findAll :: String -> String -> [String]
findAll testo modello = testo =~ modello :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- Output: ["test1","test2","test3"]
```

Ogni libreria ha le sue particolarità, ma la metodologia generale di utilizzo di `=~` per applicare la regex rimane consistente, sia che si tratti di verificare una corrispondenza o di estrarre sottostringhe. La scelta tra `regex-posix` o `regex-pcre` dipende in gran parte dalle esigenze del tuo progetto e dalle specifiche capacità regex richieste.
