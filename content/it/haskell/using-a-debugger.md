---
title:                "Utilizzo di un debugger"
aliases:
- it/haskell/using-a-debugger.md
date:                  2024-01-26T03:50:22.411272-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un debugger"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/using-a-debugger.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Usare un debugger significa immergersi nel proprio codice con strumenti progettati per ispezionare, mettere in pausa e manipolare un programma durante la sua esecuzione. I programmatori lo fanno per inseguire bug, comprendere il flusso del programma e assicurarsi che il loro codice stia facendo esattamente ciò che si aspettano.

## Come fare:
Facciamo una passeggiata con GHCi, l'ambiente interattivo di Haskell che può fungere da debugger di base. Lo avvii con il tuo codice Haskell e inizi a curiosare. Ecco un esempio:

```Haskell
main :: IO ()
main = do
    putStrLn "Ehi, come ti chiami?"
    name <- getLine
    putStrLn $ "Ciao, " ++ name ++ "! Proviamo a fare il debug."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- Fingiamo ci sia un bug qui
```

Per iniziare a fare il debug con GHCi:

```bash
$ ghci YourHaskellFile.hs
```

Imposta un breakpoint alla `buggyFunction`:

```Haskell
Prelude> :break buggyFunction
```

Esegui il tuo programma:

```Haskell
Prelude> :main
Ehi, come ti chiami?
```

Il tuo programma si mette in pausa alla `buggyFunction`. Ora puoi ispezionare le variabili, procedere passo dopo passo nel codice e valutare le espressioni.

## Approfondimento:
Storicamente, la reputazione di Haskell per le funzioni pure e la digitazione forte ha portato alla convinzione che gli strumenti di debugging fossero meno critici. La realtà è diversa: i programmi complessi beneficiano sempre di buoni strumenti di debugging. GHCi fornisce comandi di debug di base. Tuttavia, per un'esperienza più visiva o per applicazioni su larga scala, si potrebbe esplorare IDE con debugger integrati, come Visual Studio Code con estensioni Haskell o il plugin Haskell di IntelliJ.

Le alternative al debugger includono l'uso di istruzioni di stampa, noto come "debugging printf," o sfruttare il sistema di tipi forte di Haskell per rendere gli stati incorretti non rappresentabili. Tuttavia, a volte non c'è nulla che sostituisca l'avanzamento passo dopo passo nel codice.

Per quanto riguarda i dettagli implementativi, il debugger di Haskell lavora con il sistema di runtime. Può gestire breakpoint, esecuzione passo dopo passo e consentire l'ispezione delle variabili. Tuttavia, dato che Haskell viene valutato pigramente, le cose possono diventare un po' controintuitive. Fare il debug di un programma Haskell significa spesso tenere d'occhio quando e come vengono valutate le espressioni.

## Vedi anche:
- [Guida dell'utente di GHC - Debugger](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [Plugin Haskell di IntelliJ](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
