---
title:                "Trova la lunghezza di una stringa"
html_title:           "Ruby: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui è necessario trovare la lunghezza di una stringa in un programma Ruby. Ad esempio, può essere utile per verificare se una stringa è troppo lunga per essere gestita da un'operazione specifica o per assicurarsi che una stringa sia di una lunghezza specifica per soddisfare i requisiti di un'API.

## Come

Per trovare la lunghezza di una stringa in Ruby, è possibile utilizzare il metodo `length` o `size`. Vediamo un esempio utilizzando il metodo `length`:

```Ruby
nome = "Maria"
puts nome.length
```

Output:
```
5
```

Entrambi i metodi, `length` e `size`, restituiscono il numero di caratteri nella stringa. Tuttavia, il metodo `size` può essere utilizzato anche per ottenere la dimensione di una matrice o di un hash. È importante notare che entrambi i metodi sono *non distruttivi*, il che significa che non modificano la stringa originale.

## Deep Dive

Nella programmazione, una stringa è semplicemente una sequenza di caratteri - lettere, numeri o simboli - racchiusi tra le virgolette. Per trovare la sua lunghezza, Ruby scorre la stringa uno carattere alla volta finché non raggiunge la fine e aggiorna un contatore che tiene traccia del numero di caratteri.

Inoltre, è possibile utilizzare il metodo `bytesize` per ottenere la dimensione della stringa in byte. Questo è particolarmente utile quando si manipolano file o si lavora con l'encoding dei caratteri.

## Vedi Anche

- [Ruby String Class](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby Documentation](https://ruby-doc.org/)
- [Ruby Learning Resources](https://www.ruby-lang.org/en/documentation/)