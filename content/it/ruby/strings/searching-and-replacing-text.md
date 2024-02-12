---
title:                "Ricerca e sostituzione del testo"
aliases:
- /it/ruby/searching-and-replacing-text/
date:                  2024-01-20T17:58:29.391221-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Ricercare e sostituire testo significa trovare specifiche stringhe in un testo e cambiarle con altre. I programmatori lo fanno per correggere errori, aggiornare codici o manipolare dati.

## How to:
La ricerca e sostituzione in Ruby può essere semplice grazie all'uso di `gsub` e `sub`. 

Ecco un esempio:

```ruby
testo_originale = "Buongiorno, mondo! Programmiamo con Ruby."

# Sostituire 'mondo' con 'universo'
testo_modificato = testo_originale.gsub('mondo', 'universo')
puts testo_modificato
# Output: Buongiorno, universo! Programmiamo con Ruby.

# Sostituire solo la prima occorrenza di 'o' con '0'
testo_limite = testo_originale.sub('o', '0')
puts testo_limite
# Output: Bu0ngiorno, mondo! Programmiamo con Ruby.
```

## Deep Dive
La funzione `gsub`, che viene da "global substitution", è stata nel toolbox di Ruby dal suo inizio. È globale nel senso che cambia tutte le occorrenze trovate. `sub` è più limitata; cambia solo la prima occorrenza.

Inoltre, `gsub` e `sub` possono utilizzare espressioni regolari, per sostituzioni più potenti e flessibili:

```ruby
testo = "Ruby 2.6 Ruby 2.7 Ruby 3.0"
testo.gsub(/\d\.\d/) { |match| '3.1' }
# Output: Ruby 3.1 Ruby 3.1 Ruby 3.1
```

Per sostituzioni semplici, usiamo stringhe. Per complessità, meglio le regex.

## See Also
- [RubyDoc: String#sub](https://ruby-doc.org/core-2.7.0/String.html#method-i-sub)
- [RubyDoc: String#gsub](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Ruby’s Regular Expressions](https://www.rubyguides.com/2015/06/ruby-regex/)
