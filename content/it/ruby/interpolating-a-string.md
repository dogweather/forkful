---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
L'interpolazione di stringhe in Ruby è un modo per inserire variabili o espressioni all'interno di una stringa. È usata per rendere il codice più leggibile e ordinato, permettendo di combinare testo e variabili in modo diretto e senza interruzioni.

## Come si fa:
Ecco qualche esempio di interpolazione.
```Ruby 
nome = "Marco"
puts "Ciao, #{nome}!"   # => Stampa: Ciao, Marco!
```
L'interpolazione funziona anche con le espressioni.
```Ruby 
a = 5
b = 10
puts "La somma di #{a} e #{b} è #{a + b}."  # => Stampa: La somma di 5 e 10 è 15.
```
N.B.: Ricordati, l'interpolazione funziona solo con le stringhe "double-quoted"!

## Approfondimento
L'interpolazione di stringhe è stata un'aggiunta di Ruby dalla sua prima versione. È più pulita e veloce delle opzioni alternative come l'uso del metodo '+' o '<<'.
  
In termini di prestazioni, Ruby prima valuta le espressioni all'interno dell’interpolazione, poi le converte in stringhe (usando `to_s`) e infine le combina nell’ordine specificato. Questo rende l'interpolazione di stringhe estremamente efficiente.

Se per qualche motivo dovessi evitare l'interpolazione, potresti concatenare le stringhe con '+' o '<<', anche se ciò può risultare meno leggibile e più lento.

## Vedi Anche 
- La documentazione ufficiale su String Interpolation in Ruby: [https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-Strings](https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-Strings)
- Un ottimo tutorial su Ruby String Interpolation: [https://www.rubyguides.com/2018/11/ruby-string-interpolation](https://www.rubyguides.com/2018/11/ruby-string-interpolation)