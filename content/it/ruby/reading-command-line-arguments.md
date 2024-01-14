---
title:                "Ruby: Lettura degli argomenti della riga di comando"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché leggere gli argomenti della linea di comando 

Leggere gli argomenti della linea di comando è un'abilità fondamentale per chiunque stia imparando a programmare in Ruby. Questa pratica consente a un programma di ricevere input dall'utente direttamente attraverso la riga di comando, rendendo il processo di esecuzione del codice più interattivo e flessibile.

## Come leggere gli argomenti della linea di comando

Per leggere gli argomenti della linea di comando in Ruby, è necessario utilizzare l'array `ARGV`, che viene automaticamente creato all'avvio del programma e contiene tutti gli argomenti inseriti dall'utente dopo il nome del file. Ad esempio, se vogliamo stampare il primo argomento inserito dall'utente, possiamo utilizzare il seguente codice:

```Ruby
puts ARGV[0]
```

Se eseguiamo questo programma da riga di comando, ad esempio con il seguente comando:

`ruby leggi_argomenti.rb primo_argomento secondo_argomento`

verrà stampato a schermo il seguente output:

`primo_argomento`

## Approfondimenti sulla lettura degli argomenti della linea di comando

Oltre all'utilizzo dell'array `ARGV`, esistono molte altre tecniche per leggere e gestire gli argomenti della linea di comando in Ruby. Ad esempio, possiamo utilizzare il metodo `shift` per eliminare il primo elemento dell'array `ARGV` e restituirlo come output, oppure possiamo utilizzare la gemma `optparse` per creare opzioni e argomenti più avanzati.

La lettura degli argomenti della linea di comando può anche diventare più complessa quando si devono gestire tipi di dati diversi da semplici stringhe, ad esempio numeri o array. Per questo è importante approfondire i concetti di casting e parsing per assicurarsi che gli input inseriti dall'utente siano correttamente convertiti nei tipi di dati desiderati.

## Vedi anche

- [Documentazione ufficiale di Ruby sugli argomenti della linea di comando](https://ruby-doc.org/core-2.7.3/doc/ARGV.html)
- [Tutorial su come leggere gli argomenti della linea di comando in Ruby](https://www.rubyguides.com/2018/12/ruby-command-line-arguments/)
- [Gemma optparse per gestire opzioni e argomenti avanzati](https://rubygems.org/gems/optparse)