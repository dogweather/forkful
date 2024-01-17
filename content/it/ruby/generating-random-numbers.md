---
title:                "Generazione di numeri casuali"
html_title:           "Ruby: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

Cosa sono i numeri casuali e perché i programmatori li utilizzano?

I numeri casuali sono numeri generati in modo casuale e imprevedibile. I programmatori li utilizzano per aggiungere un elemento di casualità ai loro programmi, come ad esempio nei giochi o nelle simulazioni. Inoltre, possono essere utilizzati per generare dati di test o per criptare informazioni.

Come: Esempi pratici e output dei codici

```Ruby
# Generare un numero casuale intero tra 1 e 100
rand(1..100)

# Generare un numero decimale casuale tra 0 e 1
rand()

# Simulare il lancio di un dado con 6 facce
rand(1..6)

# Esempio di output
=> 54
=> 0.375
=> 5
```

```Ruby
# Generare un colore casuale in formato esadecimale
('%06x' % (rand * 0xffffff))
# Esempio di output
=> ff0080
```

Deep Dive: Contesto storico, alternative e dettagli di implementazione

Il primo algoritmo per la generazione di numeri casuali è stato sviluppato da John von Neumann nel 1946 utilizzando un registro a scorrimento. Negli anni successivi, sono stati sviluppati diversi algoritmi sempre più sofisticati, come il Mersenne Twister utilizzato attualmente in Ruby. Altre alternative per la generazione di numeri casuali includono l'utilizzo di dati di input imprevedibili, come il rumore atmosferico o i movimenti del mouse. 

Un dettaglio importante è che i numeri generati non sono veramente casuali in quanto il computer utilizza algoritmi per crearli. Pertanto, non dovrebbero essere utilizzati per scopi critici come l'implementazione di algoritmi crittografici o la gestione di dati sensibili.

Vedi anche:

- [La documentazione di Ruby sulla generazione di numeri casuali](https://ruby-doc.org/core-2.7.2/Random.html)
- [Un articolo sull'importanza della generazione di numeri casuali sicuri](https://www.numericacorp.com/resources/blog/properly-implementing-random-number-generation/)
- [Un tutorial su come utilizzare i numeri casuali nei tuoi programmi Ruby](https://www.tutorialspoint.com/ruby/ruby_random_numbers.htm)