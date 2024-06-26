---
date: 2024-01-20 17:46:08.650962-07:00
description: "How to: La manipolazione di stringhe in Haskell, come l'estrazione di\
  \ sottostringhe, si appoggia spesso a funzioni di alto livello fornite da `Data.List`\u2026"
lastmod: '2024-04-05T22:50:57.279582-06:00'
model: gpt-4-1106-preview
summary: La manipolazione di stringhe in Haskell, come l'estrazione di sottostringhe,
  si appoggia spesso a funzioni di alto livello fornite da `Data.List` e altri moduli.
title: Estrazione di sottostringhe
weight: 6
---

## How to:
Ecco come si fa in Haskell:

```Haskell
import Data.List (isPrefixOf)

-- Estrarre con ‘take’ e ‘drop’
estraiSubstring :: Int -> Int -> String -> String
estraiSubstring da quanti str = take quanti . drop da $ str

-- Uso di ‘splitAt’ per dividere e prendere il secondo pezzo
secondoPezzo :: Int -> String -> String
secondoPezzo da str = snd . splitAt da $ str

-- Filtrare con una condizione (ad esempio prefisso con 'isPrefixOf')
filtrareConPrefisso :: String -> [String] -> [String]
filtrareConPrefisso prefisso = filter (prefisso `isPrefixOf`)

```

Esempio di output:

```Haskell
main = do
    let esempioString = "Questa è una stringa di esempio"
    print $ estraiSubstring 7 9 esempioString -- "una stri"
    print $ secondoPezzo 15 esempioString     -- "stringa di esempio"
    print $ filtrareConPrefisso "Questa" ["Questa è", "Quella è", "Questa stringa"] -- ["Questa è", "Questa stringa"]
```

## Deep Dive
La manipolazione di stringhe in Haskell, come l'estrazione di sottostringhe, si appoggia spesso a funzioni di alto livello fornite da `Data.List` e altri moduli. Piuttosto che iterare su una stringa, Haskell, essendo funzionale e pigro, tende a ragionare in termini di trasformazioni e filtri.

In contesti storici, linguaggi come C mettevano l'accento su loop e puntatori per manipolare stringhe. Haskell si differenzia per il suo approccio più sicuro e dichiarativo. 

Alternativamente, esistono librerie come `text` e `bytestring` per lavorare con stringhe in modo più efficiente, specialmente con grandi quantità di dati o esigenze di performance.

L'implementazione dell'estrazione di sottostringhe deve tener conto della rappresentazione di una stringa in Haskell, ovvero una lista di caratteri. Ogni operazione sulla stringa può trasformarsi in operazioni sulla lista, che è un concetto centrale nella programmazione funzionale.

## See Also
- Haskell Wiki su stringhe: https://wiki.haskell.org/Working_with_strings
- Documentazione su `Data.Text`: https://hackage.haskell.org/package/text
- Documentazione su `Data.ByteString`: https://hackage.haskell.org/package/bytestring

Queste risorse sono perfette per approfondire la manipolazione di stringhe e conoscere le alternative al tipo `String` di Haskell.
