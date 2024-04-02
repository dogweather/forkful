---
date: 2024-01-20 18:03:41.789285-07:00
description: "Iniziare un nuovo progetto in Haskell significa digitare alcuni comandi\
  \ per creare una struttura di base. I programmatori lo fanno per avere un ambiente\u2026"
lastmod: '2024-03-13T22:44:43.475686-06:00'
model: gpt-4-1106-preview
summary: "Iniziare un nuovo progetto in Haskell significa digitare alcuni comandi\
  \ per creare una struttura di base. I programmatori lo fanno per avere un ambiente\u2026"
title: Avvio di un nuovo progetto
weight: 1
---

## What & Why?
Iniziare un nuovo progetto in Haskell significa digitare alcuni comandi per creare una struttura di base. I programmatori lo fanno per avere un ambiente organizzato e pronto per lo sviluppo fin dal primo momento.

## How to:
```Haskell
-- Installa l'ultima versione dello stack
$ curl -sSL https://get.haskellstack.org/ | sh

-- Crea un nuovo progetto Haskell
$ stack new mio_progetto

-- Naviga nella directory del progetto e compila
$ cd mio_progetto
$ stack setup
$ stack build

-- Esegui il programma
$ stack exec mio_progetto-exe
```
Output di esempio:
```
Hello, Haskell!
```

## Deep Dive
Haskell è stata sviluppata negli anni '80 come un linguaggio di programmazione funzionale puro. Lo stack, introdotto più tardi, è uno strumento per costruire progetti Haskell che gestisce le dipendenze in maniera isolata. Ciò significa che puoi avere versioni diverse della stessa libreria in progetti diversi senza conflitti. Prima dello stack, il piu famoso strumento di costruzione era Cabal, ancora usato, ma Stack offre una migliore gestione delle dipendenze e delle versioni. Iniziare un progetto con Stack consente di sfruttare questi vantaggi e di avere un ambiente di sviluppo standardizzato.

## See Also
- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- [Haskell Getting Started Guide](https://www.haskell.org/downloads/)
