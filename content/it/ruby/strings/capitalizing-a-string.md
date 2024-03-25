---
title:                "Maiuscolizzare una stringa"
date:                  2024-03-25T17:31:57.504095-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è & Perché?
Rendere maiuscola una stringa significa solitamente convertire il primo carattere di una stringa in maiuscolo e tutti gli altri in minuscolo. Tuttavia, a volte può significare semplicemente assicurarsi che il primo carattere sia maiuscolo, lasciando inalterata la restante parte della stringa. Onestamente, a mio parere, è un termine alquanto vago.

## Come fare:
Ruby fornisce [metodi diretti per la manipolazione delle stringhe](https://docs.ruby-lang.org/en/3.3/String.html), inclusa la capitalizzazione:

```ruby
# Metodo integrato di Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Molto pratico.

Il metodo `.capitalize` di Ruby è conveniente ma trasforma in maiuscolo solo la prima lettera. Per un maggior controllo o per rendere maiuscole tutte le parole di una stringa (noto come "title case"), potresti voler usare il metodo `titleize` dall'estensione ActiveSupport di Rails, oppure implementarlo tu stesso:

```ruby
# Usando 'titleize' di ActiveSupport in Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Una soluzione fai-da-te
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Questo metodo divide la stringa in un array di parole, rende maiuscola ciascuna di esse, e poi le riunisce insieme con uno spazio.

Personalmente, porto avanti questa idea molto più nel mio codice. Ho scritto il mio proprio metodo [`titleize` che tiene conto delle parole brevi come "a" e "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
