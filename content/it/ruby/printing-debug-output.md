---
title:    "Ruby: Stampa dell'output di debug"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# Perché

Stampare l'output di debug è un'attività essenziale per la programmazione in Ruby. Con il debug output, è possibile visualizzare il valore delle variabili e monitorare l'esecuzione del codice, facendo sì che il processo di debugging sia più semplice e meno frustrante.

# Come fare

Per stampare l'output di debug, utilizzare il metodo "puts" seguito dal messaggio o variabile che si desidera visualizzare. Ecco un semplice esempio di codice:

```ruby
message = "Benvenuti alla mia prima post di blog in Ruby"
puts message
```

L'output di questo codice sarà "Benvenuti alla mia prima post di blog in Ruby". Inoltre, è possibile stampare più di una variabile nello stesso momento, separandole con una virgola all'interno delle parentesi di "puts". Ad esempio:

```ruby
nome = "Marco"
cognome = "Rossi"
puts nome, cognome
```

L'output di questo codice sarà "Marco" e "Rossi" in righe separate.

# Approfondimento

Ci sono molti metodi per stampare output di debug in Ruby, come ad esempio il metodo "p" che mostra anche il tipo di variabile. Inoltre, è possibile utilizzare il gem "pry" che offre un ambiente di debugging interattivo. È anche possibile creare condizioni personalizzate per visualizzare l'output di debug solo se un determinato criterio viene soddisfatto. Per saperne di più su queste tecniche e altre, consulta i link nella sezione "Vedi anche" di seguito.

# Vedi anche

- [Ruby Debugging Basics](https://ruby-doc.org/core-2.7.1/doc/debugger.html)
- [Ruby for Beginners](https://ruby-for-beginners.rubymonstas.org/index.html)
- [Debugging Techniques in Ruby](https://www.rubyguides.com/2019/10/debugging-techniques-ruby/) (in inglese)