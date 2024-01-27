---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) permette di separare i messaggi di errore dall'output normale del programma. I programmatori lo fanno per rendere più semplice il debugging e l'analisi dei log.

## How to:
Per scrivere su stderr in Ruby, usa `$stderr.puts`. Ecco un esempio:

```Ruby
puts "Questo va su standard output."
$stderr.puts "Questo è un messaggio di errore su standard error."
```

Output atteso:
```
Questo va su standard output.
Questo è un messaggio di errore su standard error.
```

## Deep Dive
Historically, stderr è stato introdotto nella programmazione UNIX per differenziare output normale ed errori. In Ruby, `STDERR` è una costante globale che rappresenta lo standard error stream. In alternativa, puoi usare `STDERR.puts` oppure `warn` per avere un output simile. L'implementazione di questi metodi invia i messaggi direttamente al file descriptor di stderr del processo corrente.

## See Also
Per esplorare ulteriormente:

- La documentazione Ruby su I/O: [Ruby Doc I/O](https://ruby-doc.org/core-3.1.0/IO.html)
- Approfondimenti su stdout e stderr: [Standard Streams - Wikipedia](https://en.wikipedia.org/wiki/Standard_streams)
- Best practice per logging in Ruby: [Ruby Logger Class](https://ruby-doc.org/stdlib-3.1.0/libdoc/logger/rdoc/Logger.html)
