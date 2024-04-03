---
date: 2024-01-26 04:18:02.495180-07:00
description: "Utilizzare un shell interattivo, o un Loop di Lettura-Valutazione-Stampa\
  \ (REPL), permette di programmare interattivamente. I programmatori lo utilizzano\u2026"
lastmod: '2024-03-13T22:44:43.771855-06:00'
model: gpt-4-0125-preview
summary: Utilizzare un shell interattivo, o un Loop di Lettura-Valutazione-Stampa
  (REPL), permette di programmare interattivamente.
title: Utilizzo di un interprete interattivo (REPL)
weight: 34
---

## Cos'è & Perché?
Utilizzare un shell interattivo, o un Loop di Lettura-Valutazione-Stampa (REPL), permette di programmare interattivamente. I programmatori lo utilizzano per testare rapidamente frammenti di codice Swift, per fare debug, o per imparare il linguaggio.

## Come fare:
Invoca REPL aprendo un terminale e eseguendo `swift`. Digita direttamente il codice e premi Invio per eseguirlo. Eccoti un assaggio:

```Swift
1> let saluto = "Ciao, REPL!"
saluto: String = "Ciao, REPL!"
2> print(saluto)
Ciao, REPL!
```

Esci con `:quit` o `Control-D`.

## Approfondimento
Le origini di REPL risalgono agli interpreti Lisp degli anni '60. Il REPL di Swift si basa su LLVM, un potente framework di compilazione, offrendo più della semplice interpretazione—è uno strumento completo con autocompletamento, debug e altro. REPL è ottimo per imparare o per prototipare, ma non è un ambiente di sviluppo autonomo. Alcune persone preferiscono usare i Playground in Xcode per un approccio più grafico e basato sui file, mentre altre si affidano alla tradizionale modifica ed esecuzione degli script.

Sotto il cofano, il REPL di Swift compila dinamicamente il codice in linguaggio macchina ed esegue, motivo per cui è relativamente veloce. Può anche accedere a qualsiasi modulo Swift compilato, o persino a librerie C, rendendolo piuttosto potente. Da notare, tuttavia, che non tutto funziona perfettamente in REPL; alcune funzionalità di Swift, in particolare quelle che richiedono configurazioni di progetto complesse o file storyboard, qui non funzioneranno.

## Vedi Anche
- [Swift.org - Iniziare](https://www.swift.org/getting-started/#using-the-repl)
- Introduzione di Apple ai [Playgrounds di Xcode](https://developer.apple.com/videos/play/wwdc2014/408/)
- [Progetto LLVM](https://llvm.org/)
