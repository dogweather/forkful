---
title:                "Ruby: Estrazione di sottostringhe"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Quando si lavora con stringhe di testo in Ruby, può essere utile e necessario estrarre sottotesti o parti di una stringa. Ciò può essere fatto con il metodo `[]`, che accetta un'argomento di inizio e uno di fine per indicare quali caratteri estrarre dalla stringa originale. Questo può essere particolarmente utile quando si lavora con grandi quantità di dati e si vuole per esempio ottenere solo un determinato nome o una data da una stringa più grande. Inoltre, l'estrazione di sottostringhe è un concetto comune in molti linguaggi di programmazione, quindi comprendere come farlo in Ruby può essere utile per sviluppatori che lavorano con diversi linguaggi.

## Come fare

Per estrarre una sottostringa in Ruby, si può utilizzare il metodo `[]`, che è disponibile per ogni stringa. Ad esempio, se si ha una stringa con il testo "Ciao amici", si può usare il seguente codice per ottenere solo la parola "amici" dalla stringa:

```Ruby
stringa = "Ciao amici"
sottostringa = stringa[5,6]
puts sottostringa
```
Questo codice stamperà "amici" nel terminale. L'indice 5 indica il primo carattere della sottostringa da estrarre, mentre il numero 6 indica quanti caratteri vengono estratti a partire da quell'indice. In questo caso, "amici" è il sesto carattere della stringa originale e ha una lunghezza di 6 caratteri.

Un altro modo per estrarre sottostringhe è utilizzare l'operatore di range `..`. Ad esempio, se si vuole estrarre i primi 3 caratteri dalla stringa "Ciao amici", si può utilizzare il seguente codice:

```Ruby
stringa = "Ciao amici"
sottostringa = stringa[0..2]
puts sottostringa
```

Il codice stamperà "Cia" nel terminale, poiché l'indice 0 è il primo carattere della stringa e l'indice 2 è l'ultimo carattere da includere nella sottostringa.

## Approfondimento

Il metodo `[]` per estrarre sottostringhe in Ruby è in realtà una scorciatoia per l'utilizzo del metodo `slice`, che accetta gli stessi argomenti. Inoltre, il metodo `slice` può essere usato anche per sostituire i caratteri della stringa con altri.

Un altro concetto importante da capire è che gli indici delle stringhe in Ruby iniziano sempre da 0. Quindi, il primo carattere di una stringa ha indice 0, il secondo ha indice 1 e così via. Questo può sembrare strano a chi viene da altri linguaggi di programmazione, ma è importante tenere a mente quando si lavora con le sottostringhe.

## Guarda anche

- [Documentazione ufficiale di Ruby per il metodo `[]`](https://ruby-doc.org/core-2.7.2/String.html#method-i-5B-5D)
- [Tutorial su come lavorare con le stringhe in Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Esempi di codice su come estrarre e manipolare le sottostringhe in Ruby](https://www.rubyguides.com/2015/09/ruby-string-methods/)