---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Ricerca e sostituzione del testo è l'opzione che ci permette di trova un particolare pezzo di codice e sostituire con un altro. I programmatori lo fanno per risparmiare tempo e evitare errori manuali.

## Come fare:

Il metodo `gsub` in Ruby ci permette di cercare e sostituire il testo. Vediamo come funziona.

```Ruby
testo = 'Ciao Mondo'
testo.gsub!('Mondo', 'Ruby')
puts testo
```
L'uscita di questo codice sarà:
```
Ciao Ruby
```
`gsub!` è la versione "in place" del metodo `gsub`, che modifica la stringa originale invece di restituire una copia modificata.

## Approfondimenti:

La funzionalità di ricerca e sostituzione del testo è fondamentale nella programmazione sin dalla sua nascita. L'alternativa a `gsub` è utilizzare un ciclo for insieme ai metodi `index` e `[]=` di String, ma è molto più laborioso.

In termini di implementazione, `gsub` utilizza un automa a stati finiti per la ricerca del testo. In pratica, ciò significa che `gsub` è molto efficiente per le operazioni di ricerca e sostituzione su stringhe lunghe.

## Riferimenti utili:

Per approfondimenti su questo argomento, date un'occhiata a questi link:

1. Documentazione Ruby - [String#gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
2. Tutorial Ruby- [Strings](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
3. Stackoverflow - [How to replace a string in place in Ruby?](https://stackoverflow.com/questions/612189/how-do-i-automatically-replace-a-string-in-place-in-ruby)