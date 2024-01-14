---
title:                "Ruby: Generazione di numeri casuali"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché programmare in Ruby?

Ci sono molte lingue di programmazione disponibili, ma perché dovresti scegliere Ruby? Se sei un appassionato di giochi o di statistiche, forse hai bisogno di un modo per generare numeri casuali. Ruby offre una varietà di metodi per generare numeri casuali, che possono essere utilizzati in una varietà di applicazioni, dall'analisi statistica al testing di giochi.

## Come generare numeri casuali in Ruby

Per generare un numero casuale in Ruby, puoi utilizzare il metodo `rand()`. Questo metodo restituirà un numero casuale compreso tra 0 e 1. Ad esempio:

```Ruby
puts rand()
```

Questo codice restituirà un numero casuale come 0.6717491234.

Se desideri ottenere un numero casuale compreso in un determinato intervallo, puoi utilizzare il metodo `rand(x)`, dove `x` è il numero massimo che desideri includere nell'intervallo. Ad esempio, se vuoi un numero casuale compreso tra 1 e 10, puoi utilizzare `rand(10)`.

```Ruby
puts rand(10)
```

Questo genererà un numero casuale tra 1 e 10, ad esempio 7.

Inoltre, puoi utilizzare il metodo `rand(x..y)`, dove `x` è il numero minimo e `y` è il numero massimo dell'intervallo. Questo metodo restituirà un numero casuale compreso tra questi due valori. Ad esempio:

```Ruby
puts rand(5..10)
```

Questo genererà un numero casuale tra 5 e 10, ad esempio 8.

## Approfondimento sulla generazione di numeri casuali

Il metodo `rand()` utilizza un algoritmo per generare numeri casuali basato sul tempo corrente, quindi il risultato sarà diverso ogni volta che verrà eseguito il codice. Tuttavia, esiste anche un altro metodo chiamato `srand()` che può essere utilizzato per "iniziare" l'algoritmo con un numero casuale di tua scelta.

Ad esempio, se desideri generare sempre lo stesso numero casuale ogni volta che viene eseguito il codice, puoi utilizzare `srand()` per impostare un numero seed:

```Ruby
srand(1234)
puts rand()
```

Il risultato restituito da questo codice sarà sempre lo stesso, ogni volta che viene eseguito.

Inoltre, Ruby offre anche la classe `Random` che ti permette di avere maggior controllo sulla generazione di numeri casuali. Ad esempio, puoi specificare il numero seed e il tipo di generatore di numeri casuali da utilizzare.

## Vedi Anche

- [Guida alla generazione di numeri casuali in Ruby](https://www.rubyguides.com/2018/07/ruby-random/)
- [Documentazione su `rand()`](https://ruby-doc.org/core-3.0.0/Random.html#method-i-rand)
- [Documentazione su `Random`](https://ruby-doc.org/stdlib-3.0.0/libdoc/securerandom/rdoc/Random.html)