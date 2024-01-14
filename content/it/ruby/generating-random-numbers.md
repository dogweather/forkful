---
title:    "Ruby: Generazione di numeri casuali"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Perché generare numeri casuali in Ruby?

Generare numeri casuali è una funzione essenziale della programmazione, specialmente in situazioni in cui è necessario simulare eventi casuali o creare dati pseudo-casuali per test. In Ruby, esistono diverse opzioni per generare numeri casuali, ognuna con una sua precisa funzione.

## Come generare numeri casuali in Ruby

Per generare un numero casuale in Ruby, è possibile utilizzare il metodo `rand`, fornendo un range di numeri come parametro. Ad esempio, per generare un numero casuale compreso tra 1 e 10, possiamo scrivere:

```
ruby
puts rand(1..10)
```

Questa istruzione restituirà un numero casuale ogni volta che viene eseguita, come ad esempio `7` o `2`.

È inoltre possibile impostare un seme con il metodo `srand` per generare una sequenza di numeri casuali riproducibile. Ad esempio:

```
ruby
srand(123)
puts rand(1..10)
puts rand(1..10)
```

Questo codice restituirà sempre gli stessi due numeri casuali, che in questo caso saranno `9` e `1`. Cambiando il seme, è possibile ottenere una sequenza diversa di numeri casuali.

## Approfondimento sulla generazione di numeri casuali in Ruby

Internamente, Ruby utilizza un generatore di numeri pseudo-casuali basato sull'algoritmo Mersenne Twister. Questo è un algoritmo molto efficiente e affidabile per generare numeri casuali. Tuttavia, è importante notare che non è completamente casuale, ma piuttosto pseudo-casuale, il che significa che è possibile indovinare il prossimo numero nella sequenza se ne conosciamo abbastanza.

Inoltre, il metodo `rand` in Ruby restituisce numeri in virgola mobile, quindi se si ha bisogno di un intero, è necessario utilizzare il metodo `rand` con il metodo `to_i`. Ad esempio:

```
ruby
puts rand(1..10).to_i
```

Infine, è importante prestare attenzione ai semi quando si utilizza il metodo `srand` per garantire la riproducibilità dei numeri casuali. Se si utilizzano semi diversi, verranno generate sequenze di numeri casuali completamente diverse.

# Vedi anche

- [Documentazione ufficiale di Ruby su Generazione di numeri casuali](https://ruby-doc.org/core-3.0.1/Random.html)
- [Articolo su Mersenne Twister su Wikipedia (in italiano)](https://it.wikipedia.org/wiki/Mersenne_Twister)