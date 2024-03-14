---
date: 2024-01-26 04:14:54.755233-07:00
description: "Un guscio interattivo, o REPL (Read-Eval-Print Loop), in Haskell ti\
  \ permette di eseguire frammenti di codice dal vivo. \xC8 un campo di gioco per\
  \ avere\u2026"
lastmod: '2024-03-13T22:44:43.476579-06:00'
model: gpt-4-0125-preview
summary: "Un guscio interattivo, o REPL (Read-Eval-Print Loop), in Haskell ti permette\
  \ di eseguire frammenti di codice dal vivo. \xC8 un campo di gioco per avere\u2026"
title: Utilizzo di un interprete interattivo (REPL)
---

{{< edit_this_page >}}

## Cosa & Perché?
Un guscio interattivo, o REPL (Read-Eval-Print Loop), in Haskell ti permette di eseguire frammenti di codice dal vivo. È un campo di gioco per avere feedback rapidi, testare funzioni e imparare il linguaggio.

## Come fare:
Per avviare il GHCi (ambiente interattivo del Glasgow Haskell Compiler), digita semplicemente `ghci` nel tuo terminale. Ecco come usarlo:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

L'output di esempio spiega che `x` è una variabile numerica e mostra che raddoppiandola si ottiene 10.

## Approfondimento:
Il GHCi di Haskell ha fatto molta strada dalla sua creazione. Offre un ricco insieme di funzionalità come il completamento automatico con tab, input su più righe e caricamento di pacchetti. Alternative come Hugs sono ora per lo più storiche, con il GHCi che è diventato lo standard. GHCi compila il codice al volo ogni volta che inserisci un'espressione, offrendoti un modo efficiente per testare il tuo codice Haskell.

## Vedi Anche:
- [La Guida per l'Utente di GHC – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Learn You a Haskell for Great Good! – Iniziare](http://learnyouahaskell.com/starting-out#hello-world)
- [Wiki di Haskell – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
