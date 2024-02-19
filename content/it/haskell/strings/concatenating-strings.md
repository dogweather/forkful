---
aliases:
- /it/haskell/concatenating-strings/
date: 2024-01-20 17:34:59.763620-07:00
description: "Concatenare le stringhe significa unire due o pi\xF9 testi in uno. Lo\
  \ facciamo per costruire messaggi, visualizzare dati o generare codice in modo dinamico."
lastmod: 2024-02-18 23:08:55.922420
model: gpt-4-1106-preview
summary: "Concatenare le stringhe significa unire due o pi\xF9 testi in uno. Lo facciamo\
  \ per costruire messaggi, visualizzare dati o generare codice in modo dinamico."
title: Concatenazione di stringhe
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Concatenare le stringhe significa unire due o più testi in uno. Lo facciamo per costruire messaggi, visualizzare dati o generare codice in modo dinamico.

## How to: (Come fare:)
In Haskell, puoi unire le stringhe usando l'operatore `(++)` oppure la funzione `concat`:

```haskell
main :: IO ()
main = do
    let saluto = "Ciao"
    let nome = "Mondo"
    putStrLn (saluto ++ " " ++ nome)

    let listaDiParole = ["Haskell", "è", "fantastico!"]
    putStrLn (concat listaDiParole)
```

Output:
```
Ciao Mondo
Haskellèfantastico!
```

Nota: per aggiungere spazi tra le parole con `concat`, usa la funzione `unwords`.

## Deep Dive (Approfondimento)
Concatenare le stringhe in Haskell è semplice ma dobbiamo prestare attenzione all'efficienza. L'operatore `(++)` ha complessità lineare rispetto alla lunghezza della prima stringa, quindi concatenare molte stringhe può diventare costoso.

Alternativa: `Data.Text` fornisce un modulo ottimizzato per lavorare con il testo dove le operazioni di concatenazione sono più efficienti rispetto alla manipolazione delle stringhe standard di Haskell.

A livello implementativo, le stringhe in Haskell sono liste di caratteri (`[Char]`). Questa scelta è comoda ma non sempre efficiente, ecco perché `Data.Text` viene spesso preferito per un uso più intensivo.

## See Also (Consulta Anche)
- [Haskell Text](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html) per una gestione più efficiente delle stringhe.
- [Hoogle](https://hoogle.haskell.org/), un motore di ricerca per la documentazione di Haskell; cerca `++` o `concat` per trovare esempi e documenti correlati.
- [Learn You a Haskell](http://learnyouahaskell.com/), una risorsa libera per imparare Haskell da zero con molte guide sull'uso delle stringhe.
