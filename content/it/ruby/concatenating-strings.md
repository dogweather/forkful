---
title:                "Ruby: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è una delle operazioni fondamentali della programmazione, essenziale per creare stringhe più lunghe e dinamiche. Utilizzando questo concetto, è possibile manipolare e combinare diverse stringhe per ottenere l'output desiderato.

## How To

Per concatenare due o più stringhe in Ruby, è possibile utilizzare l'operatore "+" o il metodo ".concat". Vediamo un esempio:

```Ruby
nome = "Marco"
cognome = "Rossi"
saluto = "Ciao " + nome + " " + cognome
puts saluto
```

In questo caso, abbiamo utilizzato l'operatore "+" per unire le stringhe "Ciao", "Marco" e "Rossi", ottenendo l'output "Ciao Marco Rossi". È importante notare che gli spazi tra le parole sono stati aggiunti manualmente all'interno dell'operatore.

Possiamo ottenere lo stesso risultato utilizzando il metodo ".concat", che unisce le stringhe senza aggiungere alcuno spazio tra di esse:

```Ruby
nome = "Marco"
cognome = "Rossi"
saluto = "Ciao".concat(nome, cognome)
puts saluto
```

L'output sarà ancora una volta "Ciao MarcoRossi", in quanto i metodi ".concat" agiscono direttamente sulla stringa "Ciao" senza aggiungere ulteriori spazi.

Oltre all'operatore "+" e al metodo ".concat", è possibile utilizzare anche il metodo ".<<". Vediamo un altro esempio:

```Ruby
nome = "Marco"
cognome = "Rossi"
saluto = "Ciao ".concat(nome).<<(" ").concat(cognome)
puts saluto
```

In questo caso, abbiamo utilizzato il metodo ".<<" per aggiungere uno spazio tra la stringa "Ciao" e la variabile "nome", e nuovamente il metodo ".concat" per unire anche la variabile "cognome".

## Deep Dive

La concatenazione di stringhe è possibile grazie al concetto di "stringa mutabile" in Ruby. Ciò significa che le stringhe possono essere modificate e manipolate, a differenza di altri tipi di dato immutabili come i numeri interi. Questo rende la concatenazione di stringhe un'operazione semplice e molto flessibile da utilizzare.

Inoltre, è possibile concatenare non solo stringhe, ma anche altri tipi di dati come numeri o booleani. Tuttavia, in questi casi è necessario convertire i valori in stringhe utilizzando il metodo ".to_s".

## See Also

- [Documentazione di Ruby su stringhe](https://ruby-doc.org/core-2.7.1/String.html)
- [Tutorial su concatenazione di stringhe in Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Esercizi pratici di concatenazione di stringhe in Ruby](https://www.hackerrank.com/domains/ruby?filters%5Bsubdomains%5D%5B%5D=ruby-strings)