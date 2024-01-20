---
title:                "Maiuscolizzare una stringa"
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in maiuscolo. I programmatori lo fanno per questioni di formattazione, come l'inizio di una frase o nomi propri.

## How to: (Come fare:)
```Ruby
# Metodo `capitalize` su una singola parola
parola = "ruby"
puts parola.capitalize  # => "Ruby"

# Metodo `titleize` in Rails (non in Ruby standard)
require 'active_support/core_ext/string/inflections'
frase = "benvenuti al tutorial di ruby"
puts frase.titleize     # => "Benvenuti Al Tutorial Di Ruby"

# Utilizzando `split` e `map` in Ruby puro
def capitalizza_frase(frase)
  frase.split.map(&:capitalize).join(' ')
end

puts capitalizza_frase("ciao mondo")  # => "Ciao Mondo"
```

## Deep Dive (Approfondimento)
Capitalizzare le stringhe non è sempre stato così semplice; nelle prime versioni di Ruby, dovevamo scrivere metodi manualmente. Oggi, con metodi come `capitalize` e `titleize` (quest'ultimo introdotto da Rails), il processo è automatizzato.

Sotto il cofano, `capitalize` cambia la prima lettera in maiuscolo e il resto in minuscolo. Se si necessita un controllo più granulare, si possono usare i metodi `upcase` e `downcase` per trasformare le lettere come si desidera. A partire da Ruby 2.4, abbiamo anche `capitalize!` e `upcase!`, che modificano la stringa sul posto senza creare un nuovo oggetto.

Un'alternativa è l'utilizzo di espressioni regolari e il metodo `gsub` per raggiungere lo stesso scopo, ma con più controllo sulle regole di capitalizzazione.

```Ruby
# Capitalizzazione con `gsub` e regex
def capitalizza_custom(frase)
  frase.gsub(/\b\w/) { |lettera| lettera.upcase }
end

puts capitalizza_custom("ruby è divertente")  # => "Ruby È Divertente"
```

## See Also (Vedi Anche)
- Documentazione Ruby [capitalize](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- ActiveSupport [`titleize`](https://api.rubyonrails.org/classes/String.html#method-i-titleize)
- Ruby [gsub](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub) e [Regular Expressions](https://ruby-doc.org/core-2.7.0/Regexp.html)
- Stack Overflow: [How to capitalize the first letter in Ruby](https://stackoverflow.com/questions/13520162/how-to-capitalize-the-first-letter-in-ruby)