---
title:                "Ruby: Convertire una stringa in minuscolo"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Perché

Convertire una stringa in lettere minuscole può essere utile quando si vuole uniformare il testo in un documento o quando si vuole fare una ricerca su una stringa senza dover tenere conto delle lettere maiuscole.

## Come fare

Per convertire una stringa in lettere minuscole in Ruby, basta utilizzare il metodo `downcase`:

```Ruby
stringa = "Questa Stringa Ha lettere MAIUSCOLE"
puts stringa.downcase
```

Questo produrrà l'output: "questa stringa ha lettere maiuscole".

Inoltre, è possibile utilizzare questo metodo anche su una variabile che contiene un input dell'utente:

```Ruby
puts "Inserisci una parola:"
parola = gets.chomp
puts "La parola in minuscolo è: #{parola.downcase}"
```

In questo caso, l'utente inserirà una parola e il programma restituirà la stessa parola in minuscolo.

## Approfondimento

Il metodo `downcase` è un metodo di stringa che trasforma tutte le lettere in caratteri minuscoli. In confronto, il metodo `upcase` trasforma le lettere in caratteri maiuscoli.

Inoltre, è importante notare che in Ruby esiste il concetto di "codifica" di un carattere, che determina come deve essere rappresentato all'interno di una stringa. Poiché esistono diverse codifiche, può accadere che un carattere maiuscolo in una codifica venga convertito in un carattere minuscolo in un'altra.

Un altro metodo utile per trasformare una stringa è `swapcase`, che scambia le lettere maiuscole con quelle minuscole e viceversa.

## Vedi anche

- [Metodo `upcase` di Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-upcase)
- [Metodo `swapcase` di Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-swapcase)