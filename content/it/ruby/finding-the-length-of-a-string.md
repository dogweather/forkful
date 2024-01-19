---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Come Trovare la Lunghezza di una Stringa in Ruby

## Cosa e Perché?

Trovare la lunghezza di una stringa significa determinare il numero di caratteri in essa contenuti. Lo facciamo spesso per conoscere le dimensioni dei dati, limitare input dell'utente, o per manovrare stringhe in funzioni come ciclo, ecc.

## Come Fare:

In Ruby, `length` o `size` può essere utilizzato per trovare il numero di caratteri in una stringa. Di seguito sono riportati alcuni esempi:

```Ruby
str = 'Ciao Mondo'
puts str.length
# Stampa: 10

puts str.size
# Stampa: 10
```

Se vuoi contare anche gli spazi, puoi usare `str.count('^ ')`. Ecco come:

```Ruby
spazi = str.count('^ ')
puts spazi
# Stampa: 9
```

## Approfondimenti

Historicamente, `length` e `size` in Ruby sono identici, non c'è differenza tra i due. Nonostante ciò, usare un metodo rispetto all'altro dipende dalle preferenze dello sviluppatore, o spesso dallo stile del codice di un team.

Un'alternativa per trovare la lunghezza di una stringa è fare uso del metodo `count`. Tuttavia, dovresti fare attenzione a questo metodo perché `count` non conta semplicemente i caratteri, ma gli elementi in una stringa in base al set di caratteri forniti.

Per quel che riguarda i dettagli di implementazione, `length` e `size` sono implementati a livello di C e forniscono i risultati direttamente grazie all'uso di un flag di terminazione nelle stringhe, rendendo queste funzioni molto efficienti.

## Vedi Anche

Per ulteriori informazioni e dettagli, puoi fare riferimento a queste risorse:

1. Documentazione ufficiale Ruby: [https://ruby-doc.org/core-3.0.3/String.html](https://ruby-doc.org/core-3.0.3/String.html)
2. "The Ruby Way" di Hal Fulton: [https://www.amazon.com/Ruby-Way-Techniques-Programming-Addison-Wesley/dp/0321714636](https://www.amazon.com/Ruby-Way-Techniques-Programming-Addison-Wesley/dp/0321714636) 
3. "Programming Ruby 1.9 & 2.0": [https://pragprog.com/book/ruby4/programming-ruby-1-9-2-0](https://pragprog.com/book/ruby4/programming-ruby-1-9-2-0)