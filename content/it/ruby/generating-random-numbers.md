---
title:                "Ruby: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'operazione fondamentale nella programmazione, soprattutto quando si vogliono creare giochi o simulazioni. In Ruby, questo processo è semplice e veloce, rendendo il linguaggio ideale per progetti che richiedono l'utilizzo di numeri casuali.

## Come

In Ruby, esistono diverse funzioni utili per generare numeri casuali. Vediamone alcuni esempi:

```Ruby
# Genera un numero intero tra 0 e 9
rand(10)

# Genera un numero intero tra 1 e 100
rand(1..100)

# Genera un numero decimale tra 0 e 1
rand()

# Genera un numero decimale tra 0 e 10
rand * 10

# Genera un numero decimale tra 5 e 10
rand(5.0..10.0)
```

Questi sono solo alcuni esempi, ma ci sono molte altre funzioni che si possono utilizzare per generare numeri casuali in modo preciso e secondo le proprie esigenze. L'importante è conoscere i limiti di ogni funzione e saperli utilizzare nel modo corretto.

Per visualizzare i numeri casuali generati, è possibile utilizzare il metodo "puts" per stamparli a schermo. Ecco un esempio:

```Ruby
# Genera 10 numeri casuali tra 1 e 100
10.times { puts rand(1..100) }
```

Questo codice genererà una lista di 10 numeri casuali tra 1 e 100, come ad esempio:

12
48
76
33
91
66
4
27
89
55

## Deep Dive

Ora che sappiamo come generare numeri casuali in Ruby, è importante capire come funzionano queste funzioni. In realtà, i numeri che vengono generati non sono del tutto casuali, ma seguono una sequenza determinata. Per questo motivo, è consigliabile utilizzare un "seed" prima di generare i numeri casuali.

Il "seed" è un numero utilizzato come inizializzazione per l'algoritmo di generazione dei numeri casuali. Di default, Ruby utilizza il tempo corrente come "seed", ma è possibile specificare un "seed" personalizzato utilizzando il metodo "srand". In questo modo, ogni volta che si utilizza il medesimo "seed", i numeri casuali generati saranno sempre gli stessi.

```Ruby
# Imposta un "seed" personalizzato a 12345
srand(12345)

# Genera 10 numeri casuali tra 1 e 100 con lo stesso "seed"
10.times { puts rand(1..100) }
```

Questo codice produrrà sempre gli stessi 10 numeri casuali, a differenza del primo esempio in cui ogni volta si otterranno numeri diversi.

## Vediamo anche

Per ulteriori informazioni sulla generazione di numeri casuali in Ruby, vi consiglio di consultare questi utili link:

- [Documentazione ufficiale di Ruby su rand](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-rand)
- [Articolo su randomizzazione e "seed" in Ruby](https://onebitcode.com/ruby-random/)
- [Tutorial su numeri casuali in Ruby](https://www.rubyguides.com/2019/01/ruby-random/)