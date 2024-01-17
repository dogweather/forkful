---
title:                "Lettura degli argomenti della linea di comando"
html_title:           "Ruby: Lettura degli argomenti della linea di comando"
simple_title:         "Lettura degli argomenti della linea di comando"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Leggere gli argomenti della riga di comando è una pratica comune nella programmazione, che consiste nel raccogliere gli input forniti dall'utente quando il programma viene eseguito. Questi argomenti possono essere utilizzati per personalizzare l'esecuzione del programma o per passare informazioni importanti alla logica del codice.

## Come fare:
Per leggere gli argomenti della riga di comando in Ruby, è necessario utilizzare un'apposita funzione chiamata ```ARGV```, che restituisce un array contenente tutti gli argomenti passati all'avvio del programma. Di seguito un esempio di codice:

```
# Legge il primo argomento passato alla riga di comando e lo salva in una variabile
nome = ARGV[0]

# Stampa il nome utente sulla console
puts "Ciao #{nome}! Benvenuto!"
```

Esempio di output:

```
$ ruby benvenuto.rb Maria
Ciao Maria! Benvenuto!
```

## Approfondimento:
Leggere gli argomenti della riga di comando è una funzionalità che esiste sin dai primi linguaggi di programmazione a riga di comando come C e Unix shell. In Ruby, i programmatori possono utilizzare anche il modulo ```OptionParser``` per gestire gli argomenti da riga di comando in maniera più strutturata. Inoltre, gli sviluppatori possono anche creare interfacce grafiche per permettere agli utenti di interagire con il programma senza dover digitare argomenti manualmente.

## Vedi anche:
- [Documentazione di Ruby sull'utilizzo della funzione ```ARGV```](https://rubyreferences.github.io/rubyref/language/arguments.html)
- [Documentazione di Ruby sul modulo ```OptionParser```](https://rubyreferences.github.io/rubyref/command_line/optionparser.html)
- [Esempi di interfacce grafiche in Ruby](https://github.com/TK-Nguyen/Tk/blob/master/README.md)