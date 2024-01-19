---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Convertire una stringa in minuscolo significa trasformare tutti i suoi caratteri in lettere minuscole. I programmatori lo fanno per normalizzare i dati di input, facilitare il confronto tra stringhe, e per evitare errori dovuti a casi diversi.

## Come si fa:
In Ruby, la conversione di una stringa in minuscolo è semplice grazie al metodo `downcase`. Ecco un esempio:

```Ruby
stringa = "Ciao, Mondo!"

stringa_minuscola = stringa.downcase

puts stringa_minuscola
```

L'output sarà:

```Ruby
"ciao, mondo!"
```

## Approfondimento:
Originariamente, le operazioni sui casi delle stringhe erano necessarie a causa delle differenze tra i caratteri maiuscoli e minuscoli in ASCII. Oggi, il metodo `downcase` in Ruby è fondamentale per la normalizzazione dei dati, specie nel contesto del confronto tra stringhe.

E' possibile convertire tutto in maiuscolo con l'uso del metodo `upcase`:

```Ruby
stringa = "Ciao, Mondo!"
puts stringa.upcase // Output: "CIAO, MONDO!"
```

Ruby implementa il metodo `downcase` iterando su ciascun carattere della stringa, cambiandolo in minuscolo con l'uso di un semplice mappaggio ASCII.

## Vedi anche:
- [Ruby Documentation: String](https://ruby-doc.org/core-2.7.1/String.html)
- [ASCII Code - The extended ASCII table](https://www.ascii-code.com/)
- [Ruby - Strings](https://www.tutorialspoint.com/ruby/ruby_strings.htm)