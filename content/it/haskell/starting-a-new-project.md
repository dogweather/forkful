---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Iniziare un nuovo progetto di programmazione in Haskell significa creare un nuovo ambiente di lavoro dove svilupperemo le nostre funzioni e i nostri moduli. Lo facciamo per strutturare il nostro codice in modo coerente e organizzato, facilitando così la manutenzione e lo sviluppo del progetto.

## Come fare:

Per iniziare un nuovo progetto in Haskell, usiamo Stack, uno strumento di sviluppo multiplo. Ecco un esempio di come creare un nuovo progetto:

```Haskell
$ stack new MyProject
```

Questo comando creerà una nuova cartella chiamata "MyProject" con tutti i file necessari. Per esempio, avremo un file .cabal, un file stack.yaml, e una directory src con un file Main.hs.

Possiamo compilare ed eseguire il nostro progetto utilizzando i comandi:

```Haskell
$ stack build
$ stack exec MyProject-exe
```

L'output sarà "someFunc".

## Approfondimenti

Negli anni, ci sono stati vari modi per creare progetti Haskell, come Cabal, per esempio. Tuttavia, Stack è diventato il sistema preferito da molti sviluppatori per la sua robustezza e facilità d'uso. 

Iniziare un nuovo progetto con Stack significa anche scegliere una specifica versione del compilatore GHC, che può essere definita nel file stack.yaml. Questo permette semplicità e coerenza tra vari sviluppatori di uno stesso progetto.

Altro dettaglio importante è che Stack gestisce anche le dipendenze del progetto. Queste possono essere specificate nel file .cabal e verranno installate automaticamente quando si esegue `stack build`.

## Per Approfondire

Per maggiori informazioni su come iniziare un progetto Haskell con Stack, potete leggere la guida ufficiale disponibile a [questo link](https://docs.haskellstack.org/en/stable/GUIDE/). 

Per avere un'idea più chiara della struttura di un progetto Haskell, potete consultare il tutorial di Stephen Diehl [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/). 

Infine, se siete interessati alla storia e agli sviluppi futuri di Haskell, vi consiglio di leggere il libro ["Haskell: The Craft of Functional Programming"](https://www.pearson.com/us/higher-education/program/Hutton-Haskell-The-Craft-of-Functional-Programming-3rd-Edition/PGM333820.html).