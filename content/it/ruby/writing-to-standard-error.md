---
title:                "Scrivere su errore standard"
html_title:           "Ruby: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti dover scrivere sullo standard error in Ruby. Ad esempio, potresti voler segnalare un errore durante l'esecuzione del tuo programma o scrivere informazioni di debugging per aiutarti a risolvere eventuali problemi.

## Come fare

Per scrivere sullo standard error in Ruby, è possibile utilizzare il metodo `.puts` seguito da `$stderr`, che rappresenta lo standard error. Ecco un esempio:

```Ruby
puts "Questo messaggio verrà scritto sullo standard output."
$stderr.puts "Questo messaggio verrà scritto sullo standard error."
```

L'output sarà il seguente:

```
Questo messaggio verrà scritto sullo standard output.
Questo messaggio verrà scritto sullo standard error.
```

## Approfondimento

Ci sono alcune differenze fondamentali tra lo standard output e lo standard error in Ruby. Prima di tutto, lo standard output viene visualizzato a schermo, mentre lo standard error viene scritto su un file di log di errore. Inoltre, lo standard output viene mostrato solo se il programma viene eseguito correttamente, mentre lo standard error viene comunque mostrato anche in caso di errori.
È importante anche notare che lo standard output e lo standard error possono essere reindirizzati in luoghi diversi. Ad esempio, è possibile inviare lo standard output ad un file di log e lo standard error a schermo. È possibile farlo utilizzando il carattere `>` seguito dal nome del file desiderato. Ad esempio:

```
ruby programma.rb > output.log
```

Lo standard output del programma verrà ora scritto nel file `output.log`, mentre lo standard error verrà ancora mostrato a schermo.

## Vedi anche

- [Documentazione ufficiale di Ruby su $stderr](https://ruby-doc.org/core-2.7.0/IO.html#method-c-new-label-Io+from+Files)
- [Tutorial su come gestire gli errori in Ruby](https://www.rubyguides.com/2019/02/ruby-errors/)
- [Una spiegazione dettagliata delle differenze tra lo standard output e lo standard error](https://www.semicolonworld.com/question/59817/the-difference-between-puts-and-stdputs-in-ruby)