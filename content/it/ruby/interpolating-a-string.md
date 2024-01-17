---
title:                "Interpolazione di una stringa"
html_title:           "Ruby: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
 L'interpolazione di una stringa è un processo in cui le variabili e le espressioni vengono inserite in una stringa per formare una stringa finale. I programmatori lo fanno per creare stringhe dinamiche che possano cambiare in base al contesto e alle variabili che vengono utilizzate.

## Come fare:
```Ruby
nome = "Alice"
puts "Ciao, #{nome}!" # produce "Ciao, Alice!"

num1 = 5
num2 = 10
puts "#{num1} + #{num2} = #{num1 + num2}" # produce "5 + 10 = 15"
```

## Approfondimento:
L'interpolazione dei stringhe è diventata popolare con l'avvento del linguaggio di programmazione Perl negli anni '80. Altri linguaggi, come Python e Ruby, hanno anche adottato questa funzionalità. Un'alternativa all'interpolazione delle stringhe è la concatenazione di stringhe, ma l'interpolazione è preferibile perché rende il codice più facile da leggere e modificare. In Ruby, è possibile utilizzare anche il metodo `format` per interpolare le stringhe.

## Vedi anche:
- [Documentazione di Ruby sulla interpolazione delle stringhe](https://ruby-doc.org/core-3.0.0-doc/syntax/literals_rdoc.html#label-String+Interpolation)
- [Articolo su Medium su quando e perché usare l'interpolazione delle stringhe](https://medium.com/rubycademy/string-interpolation-in-ruby-fb7b3f72bd11)