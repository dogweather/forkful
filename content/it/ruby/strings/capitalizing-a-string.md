---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Capitalizzare una stringa di solito significa convertire il primo carattere\
  \ di una stringa in maiuscolo e il resto in minuscolo. Ma a volte pu\xF2\u2026"
lastmod: '2024-03-25T19:21:57.962401-06:00'
model: gpt-4-0125-preview
summary: "Capitalizzare una stringa di solito significa convertire il primo carattere\
  \ di una stringa in maiuscolo e il resto in minuscolo. Ma a volte pu\xF2\u2026"
title: Capitalizzare una stringa
---

## Cosa e Perché?
Capitalizzare una stringa di solito significa convertire il primo carattere di una stringa in maiuscolo e il resto in minuscolo. Ma a volte può significare semplicemente assicurarsi che il primo carattere sia maiuscolo lasciando invariato il resto della stringa. Onestamente, secondo me, è un termine alquanto vago.

## Come fare:
Ruby fornisce [metodi diretti per la manipolazione delle stringhe](https://docs.ruby-lang.org/en/3.3/String.html), inclusa la capitalizzazione:

```ruby
# Metodo integrato di Ruby
string = "ciao MONDO"
capitalized_string = string.capitalize
puts capitalized_string # => "Ciao mondo"
```

Molto pratico.

Il metodo `.capitalize` di Ruby è comodo ma mette in maiuscolo solo la prima lettera. Per un maggiore controllo o per capitalizzare ogni parola in una stringa (noto come caso del titolo), potresti voler utilizzare il metodo `titleize` dall'estensione ActiveSupport di Rails, o implementarlo tu stesso:

```ruby
# Usando 'titleize' di ActiveSupport in Rails
require 'active_support/core_ext/string/inflections'
string = "ciao mondo"
puts string.titleize # => "Ciao Mondo"
```

```ruby
# Una soluzione fatta in casa
string = "ciao mondo"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Ciao Mondo"
```

Questo metodo divide la stringa in un array di parole, capitalizza ciascuna di esse, poi le riunisce insieme con uno spazio.

Personalmente, porto questa idea molto più lontano nel mio codice. Ho scritto il mio [metodo `titleize` che tiene conto di parole piccole come "a" e "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
