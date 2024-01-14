---
title:    "Ruby: Cancellazione di caratteri corrispondenti a un modello"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, durante la scrittura di codice Ruby, può essere necessario eliminare determinati caratteri da una stringa o un array. Questo può essere fatto utilizzando il metodo `delete`, che ci permette di specificare un pattern da seguire per eliminare i caratteri corrispondenti.

## Come fare

Per utilizzare il metodo `delete` in Ruby, basta seguire questi semplici passaggi:

1. Definire una stringa o un array su cui vogliamo lavorare.
2. Utilizzare il metodo `delete` seguito dal pattern tra parentesi.
3. Assegnare il risultato a una nuova variabile o stamparlo direttamente.

Ecco un esempio di codice che elimina tutti i numeri dall'array:

```ruby
array_numeri = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
nuovo_array = array_numeri.delete(1..10)
puts nuovo_array
```
Output: `[]` 

Nel codice sopra, abbiamo creato un array contenente i numeri da 1 a 10 e poi abbiamo utilizzato il metodo `delete` per specificare il range di numeri da eliminare. Il nuovo array ottenuto è vuoto, perché tutti i numeri sono stati eliminati.

## Approfondimento

Il metodo `delete` può essere particolarmente utile quando si lavora con stringhe e si vuole eliminare caratteri speciali o spazi bianchi. Inoltre, può essere combinato con altri metodi di manipolazione delle stringhe come `gsub` per eseguire sostituzioni più complesse.

Un'altra caratteristica interessante è che il metodo `delete` può anche essere chiamato su una stringa senza specificare un pattern, in questo caso eliminerà tutti i caratteri duplicati.

## Vedi anche

- [Ruby String Class Docs](https://ruby-doc.org/core-2.6.3/String.html)
- [Guide to Ruby Methods](https://www.rubyguides.com/ruby-methods/)
- [Ruby Regular Expressions](https://www.rubyguides.com/2015/06/ruby-regex/)