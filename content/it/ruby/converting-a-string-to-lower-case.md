---
title:                "Ruby: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una stringa in caratteri minuscoli è utile quando si vuole avere una stringa nel formato più comune, in modo da poterla facilmente confrontare o manipolare.

## Come fare

```Ruby
stringa = "RUBY PROGRAMMAZIONE"
puts(stringa.downcase)
```

Questo codice produrrà l'output "ruby programmazione". Come si può notare, la funzione `downcase` viene applicata alla variabile `stringa` e ritorna una nuova stringa in minuscolo.

## Approfondimento

La funzione `downcase` è una caratteristica del tipo String in Ruby ed è uno dei molti metodi disponibili per manipolare le stringhe. Ci sono anche altri metodi come `upcase` per trasformare una stringa in maiuscolo e `capitalize` per far iniziare una stringa con la prima lettera maiuscola.

Inoltre, la conversione in minuscolo di una stringa può essere utile quando si lavora con input dell'utente. Ad esempio, se l'utente inserisce la stringa "ProGraMMaZIone" e si vuole che il programma funzioni correttamente anche in questo caso, si può utilizzare la funzione `downcase` per trasformare la stringa in "programmazione" e poi confrontarla con altre stringhe.

## Vedi anche

- [Documentazione Ruby sulle stringhe](https://www.ruby-lang.org/it/documentation/quickstart/2/)
- [Metodi disponibili per le stringhe in Ruby](https://www.rubyguides.com/2018/02/ruby-string-methods/)