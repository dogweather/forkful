---
title:    "Ruby: Trovare la lunghezza di una stringa"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Una delle operazioni più semplici che possiamo fare con una stringa in Ruby è trovare la sua lunghezza. Anche se sembra banale, avere la capacità di eseguire questa operazione è fondamentale per qualsiasi programmatore Ruby.

## Come fare

In Ruby, trovare la lunghezza di una stringa è molto semplice. Possiamo utilizzare il metodo `.length` diretto sulla stringa per ottenere il numero di caratteri all'interno di essa.

```ruby
stringa = "Ciao mondo"
puts stringa.length # Output: 10
```

Come vediamo nell'esempio sopra, il metodo `.length` ci restituisce il numero di caratteri all'interno della stringa, inclusi spazi e punteggiatura.

Possiamo anche utilizzare il metodo `.size` per ottenere lo stesso risultato. Questo metodo funziona esattamente come il metodo `.length` ed è solo un alias per la stessa funzione.

```ruby
stringa = "Hello world"
puts stringa.size # Output: 11
```

Infine, possiamo anche utilizzare il metodo `.bytesize` per ottenere la lunghezza della stringa in byte. Ciò può essere utile quando si lavora con stringhe di caratteri non ASCII.

```ruby
stringa = "こんにちは世界"
puts stringa.bytesize # Output: 18
```

## Approfondimento

La lunghezza di una stringa può essere influenzata da diversi fattori. Ad esempio, la codifica dei caratteri utilizzata può avere un impatto sulla dimensione della stringa in byte. Inoltre, le operazioni come l'inserimento di un carattere di escape (`\`) possono causare un aumento della lunghezza della stringa.

È importante tenere presente questi aspetti in modo da ottenere risultati accurati quando si calcola la lunghezza di una stringa.

## Vedi anche

- [Documentazione di Ruby su Stringhe](https://ruby-doc.org/core-2.7.3/String.html)
- [Metodi di stringa utili in Ruby](https://www.rubyguides.com/2016/03/ruby-string-methods/)
- [Guida completa di Ruby per principianti](https://www.codecademy.com/learn/learn-ruby)