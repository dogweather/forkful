---
title:                "Estrazione di sottostringhe"
html_title:           "Ruby: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler estrarre sottostringhe in Ruby. Forse vuoi manipolare dei dati per elaborare informazioni più specifiche, o forse vuoi semplificare il tuo codice rendendo più leggibili le tue stringhe.

## Come fare

Per estrarre una sottostringa in Ruby, possiamo utilizzare il metodo `slice` o `[]`. Ad esempio, se vogliamo estrarre una sottostringa di 3 caratteri a partire dalla terza posizione di una stringa, il nostro codice sarebbe simile a questo:

```Ruby
stringa = "Ciao a tutti!"
sottostringa = stringa.slice(2, 3)

puts sottostringa # output: "ao "
```

In questo caso, il primo numero rappresenta l'indice di partenza, mentre il secondo numero indica il numero di caratteri da estrarre. Possiamo anche utilizzare uno o più numeri negativi per estrarre una sottostringa a partire dalla fine della stringa. Ad esempio:

```Ruby
stringa = "Ciao a tutti!"
sottostringa = stringa[-6, 6]

puts sottostringa # output: "a tutti"
```

## Approfondimento

Oltre al semplice utilizzo di `slice` o `[]`, ci sono altri metodi che possiamo utilizzare per estrarre sottostringhe in Ruby. Ad esempio, il metodo `scan` può essere utilizzato per estrarre sottostringhe basate su una determinata espressione regolare. Possiamo anche utilizzare il metodo `split` per dividere una stringa in un array di sottostringhe basate su un determinato delimitatore.

## Vedi anche

- Documentazione ufficiale di Ruby: https://ruby-doc.org/core-2.7.0/String.html#method-i-slice
- Esempi pratici di estrarre sottostringhe in Ruby: https://www.rubyguides.com/2019/07/ruby-substring/