---
title:                "Ruby: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Oggi parleremo di un aspetto fondamentale della programmazione in Ruby: trovare la lunghezza di una stringa. Questa operazione è essenziale in molti casi, come ad esempio la validazione dei dati inseriti dagli utenti o la manipolazione di testo.

## Come fare

Per trovare la lunghezza di una stringa in Ruby, possiamo utilizzare il metodo `length`. Vediamo un esempio pratico:

```Ruby
nome = "Maria"
lunghezza = nome.length
puts lunghezza
```

In questo caso, il nostro output sarà `5`, poiché la stringa "Maria" è composta da 5 caratteri (M, a, r, i, a). Possiamo anche usare questo metodo su stringhe inserite dagli utenti tramite la console o ottenute da fonti esterne come un database. Ad esempio:

```Ruby
puts "Inserisci il tuo nome:"
nome = gets.chomp
lunghezza = nome.length
puts "Il tuo nome ha #{lunghezza} caratteri."
```

Se l'utente inserisce il nome "Mario", il nostro output sarà "Il tuo nome ha 5 caratteri." Questo è solo un esempio semplice, ma il metodo `length` può essere combinato con altre operazioni e condizioni per creare codice più complesso e potente.

## Approfondimento

Oltre al metodo `length`, esistono altre opzioni per trovare la lunghezza di una stringa in Ruby. Ad esempio, possiamo utilizzare il metodo `size` o la proprietà `bytesize`. Inoltre, esistono anche metodi per trovare la lunghezza di una stringa senza contare i caratteri vuoti o per contare solo parole specifiche all'interno di una stringa.

Inoltre, è importante notare che questi metodi forniscono la lunghezza della stringa in base ai caratteri, non al numero di lettere. Questo significa che, ad esempio, la stringa "à" verrà considerata come carattere singolo anziché due lettere. Se vogliamo ottenere la lunghezza corretta in base al numero di lettere, possiamo utilizzare il metodo `chars` prima di applicare `length`.

## Vedi anche

- [Documentazione di Ruby sul metodo length](https://ruby-doc.org/core-2.5.1/String.html#method-i-length)
- [Altro approfondimento su come trovare la lunghezza di una stringa in Ruby](https://www.rubyguides.com/2019/06/ruby-string-length/)
- [Approfondimento su metodi simili per ottenere la lunghezza di una stringa](https://stackoverflow.com/questions/24205439/what-is-the-difference-between-size-equalsize-and-length/24205447#24205447)