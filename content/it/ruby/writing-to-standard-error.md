---
title:                "Ruby: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può essere una pratica utile durante lo sviluppo di un programma Ruby. Quando si sta testando e debuggando il codice, stampare messaggi di errore su standard error può aiutare a identificare e risolvere eventuali problemi in modo più rapido ed efficiente.

## Come fare

Scrivere su standard error è molto semplice in Ruby. Basta utilizzare il metodo `puts` e passare il messaggio desiderato come argomento. Ad esempio:

```Ruby
puts "Questo è un messaggio di errore"
```

L'output sarà:

```Ruby
Questo è un messaggio di errore
```

## Approfondimento

Per comprendere meglio come funziona la scrittura su standard error in Ruby, è importante conoscere la differenza tra `puts` e `print`. Entrambi i metodi stampano un messaggio, ma `puts` aggiunge automaticamente una nuova riga alla fine del messaggio, mentre `print` no.

Quando si stampa su standard error, è importante includere la nuova riga `\n` per garantire che il messaggio sia correttamente visualizzato come un errore. Ad esempio:

```Ruby
print "Questo è un messaggio di errore \n"
puts "Questo è un altro messaggio di errore"
```

L'output sarà:

```Ruby
Questo è un messaggio di errore
Questo è un altro messaggio di errore
```

Inoltre, è possibile specificare il flusso di output utilizzando il carattere `>` prima del metodo `puts`. Ad esempio:

```Ruby
$stderr.puts "Questo è un messaggio di errore"
```

L'output sarà visualizzato solo su standard error.

## Vedi anche

* [Documentazione ufficiale di Ruby](https://www.ruby-lang.org/it/documentation/)
* [Scrittura su standard error in Ruby](https://www.rubyguides.com/2018/10/ruby-standard-error/)
* [RailsCast su standard error](http://railscasts.com/episodes/104-exception-notifications-revised?autoplay=true)