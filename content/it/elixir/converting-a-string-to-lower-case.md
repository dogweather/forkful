---
title:                "Convertire una stringa in minuscolo"
html_title:           "Elixir: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cosa e' e perche'? 
La conversione di una stringa in lettere minuscole e' un processo comune nella programmazione che consiste nel trasformare tutte le lettere maiuscole di una stringa in lettere minuscole. I programmatori effettuano questa conversione per uniformare il formato delle lettere in una stringa e facilitare il confronto tra diverse stringhe.

## Come si fa: 
```Elixir
stringa = "Ciao a tutti!"
stringa_min = String.downcase(stringa)
IO.puts stringa_min
```
Output: "ciao a tutti!"

## Approfondimento: 
- La conversione delle stringhe in lettere minuscole e' stata introdotta nel linguaggio di programmazione Elixir con la versione 1.2. 
- Un'alternativa alla funzione `String.downcase()` e' l'utilizzo dell'operatore `^` per rimuovere l'accento sulle lettere maiuscole in una stringa.
- L'implementazione di `String.downcase()` fa uso della libreria `Unicode` e segue le regole della [Unicode Case Mapping](https://www.unicode.org/versions/Unicode13.0.0/ch03.pdf).

## Vedi anche: 
- [Documentazione di Elixir sulle stringhe](https://hexdocs.pm/elixir/String.html)
- [Erlang Unicode module](http://erlang.org/doc/man/unicode.html)