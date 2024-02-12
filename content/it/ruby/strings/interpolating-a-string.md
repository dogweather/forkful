---
title:                "Interpolazione di una stringa"
aliases:
- /it/ruby/interpolating-a-string/
date:                  2024-01-20T17:51:49.826009-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolazione di una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
L'interpolazione di stringhe in Ruby permette di inserire dinamicamente valori all'interno di una stringa. I programmatori la usano per costruire stringhe in modo flessibile e leggermente più pulito rispetto alla concatenazione.

## How to: (Come Fare)
```Ruby
nome = "Luca"
saluto = "Ciao, #{nome}! Come stai?"
puts saluto  # => Ciao, Luca! Come stai?
```
Un altro esempio, con espressioni matematiche:
```Ruby
somma = 2 + 3
messaggio = "Il risultato di 2 + 3 è #{somma}"
puts messaggio  # => Il risultato di 2 + 3 è 5
```

## Deep Dive (Approfondimento)
L'interpolazione delle stringhe esiste sin da Ruby 1.8 e persiste come una delle caratteristiche più apprezzate. A differenza della concatenazione, che unisce stringhe e variabili con il `+`, l'interpolazione è più leggibile e performante perché avviene al momento della creazione della stringa. Invece di scrivere `"Ciao, " + nome + "!"`, puoi semplicemente usare `"Ciao, #{nome}!"`. 

In altre lingue, come Python o PHP, l’interpolazione si fa diversamente o ha sintassi differenti. In Ruby, si usa `#{...}` all’interno di stringhe delimitate da doppie virgolette. Attenzione: con virgolette singole, non funziona.

Internamente, Ruby chiama il metodo `.to_s` sull'espressione dentro `#{...}`. Ciò significa che qualunque oggetto può essere interpolato, a patto che risponda a `.to_s`. Ad esempio, se un oggetto non ha un metodo `.to_s` appropriato, Ruby usa la rappresentazione standard che generalmente include l'indirizzo in memoria dell'oggetto.

## See Also (Vedi Anche)
- Documentazione ufficiale di Ruby sulla [sintassi letterale delle stringhe](https://docs.ruby-lang.org/en/2.6.0/syntax/literals_rdoc.html#label-Strings)
- [Ruby-Doc.org](https://ruby-doc.org/core-3.1.2/String.html#method-i-25) per metodi specifici di String in Ruby
- Ruby Style Guide su GitHub per [consigli di stile](https://github.com/rubocop/ruby-style-guide#strings) nelle interpolazioni e altre pratiche di codifica in Ruby
