---
aliases:
- /it/haskell/interpolating-a-string/
date: 2024-01-20 17:51:06.457994-07:00
description: "Interpolare una stringa significa inserire dinamicamente espressioni\
  \ o variabili direttamente all'interno di una stringa di testo. I programmatori\
  \ lo\u2026"
lastmod: 2024-02-18 23:08:55.916524
model: gpt-4-1106-preview
summary: "Interpolare una stringa significa inserire dinamicamente espressioni o variabili\
  \ direttamente all'interno di una stringa di testo. I programmatori lo\u2026"
title: Interpolazione di una stringa
---

{{< edit_this_page >}}

## What & Why?
Interpolare una stringa significa inserire dinamicamente espressioni o variabili direttamente all'interno di una stringa di testo. I programmatori lo fanno per costruire stringhe in modo flessibile e leggere, adattandole in base ai dati in esecuzione.

## How to:
Haskell non ha l'interpolazione di stringa incorporata come altri linguaggi, ma possiamo ottenere un risultato simile usando la libreria `text` e il quasiquoter `str`.

```Haskell
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.String.Interpolate (i)

myName :: String
myName = "Giuseppe"

main :: IO ()
main = do
  let age = 30
  let greeting = [i|Ciao, sono #{myName} e ho #{age} anni.|]
  T.putStrLn greeting
```

Questo codice stamperà:

```
Ciao, sono Giuseppe e ho 30 anni.
```

## Deep Dive
L'interpolazione di stringa non è un concetto nativo di Haskell, un linguaggio creato nel 1990 con un focus sulla programmazione puramente funzionale. In altri linguaggi, l'interpolazione è spesso integrata direttamente nella sintassi del linguaggio. In Haskell, la funzionalità può essere raggiunta attraverso librerie di terze parti come `text` e `interpolate`.

Alternativamente, è possibile utilizzare la funzione `printf` da `Text.Printf` che offre un'interpolazione ispirata a quella del linguaggio C, oppure costruire la propria funzione di interpolazione con funzioni di formattazione basiche disponibili in Haskell.

Entrando nei dettagli di implementazione, `str` è un quasiquoter che traduce la stringa interpolata in una espressione che combina stringhe e valori. Questo avviene a tempo di compilazione. Quindi, a differenza di linguaggi come Python o JavaScript, l'interpolazione in Haskell viene risolta prima dell'esecuzione.

## See Also
- Documentazione sulla libreria `text`: https://www.stackage.org/package/text
- Documentazione sulla libreria `interpolate`: https://hackage.haskell.org/package/interpolate
- Un tutorial su `printf` in Haskell: http://zvon.org/other/haskell/Outputprelude/printf_f.html

Ricorda: Haskell è molto potente e offre diversi modi per lavorare con le stringhe, quindi esplora e trova il metodo che preferisci per le tue esigenze di interpolazione!
