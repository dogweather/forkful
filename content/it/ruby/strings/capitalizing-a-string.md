---
aliases:
- /it/ruby/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:13.688577-07:00
description: "Capitalizzare una stringa nella programmazione spesso si riferisce alla\
  \ conversione del primo carattere di una stringa in maiuscolo e il resto in\u2026"
lastmod: 2024-02-18 23:08:56.365417
model: gpt-4-0125-preview
summary: "Capitalizzare una stringa nella programmazione spesso si riferisce alla\
  \ conversione del primo carattere di una stringa in maiuscolo e il resto in\u2026"
title: Capitalizzare una stringa
---

{{< edit_this_page >}}

## Cosa & Perché?
Capitalizzare una stringa nella programmazione spesso si riferisce alla conversione del primo carattere di una stringa in maiuscolo e il resto in minuscolo. I programmatori fanno ciò per motivi quali aderire a convenzioni di denominazione, rendere gli output più leggibili o garantire la coerenza dei dati per comparazioni e memorizzazione.

## Come Fare:
Ruby offre metodi semplici per la manipolazione delle stringhe, inclusa la capitalizzazione. Ecco come puoi capitalizzare una stringa in Ruby:

```ruby
# Metodo integrato di Ruby
stringa = "ciao mondo"
stringa_capitalizzata = stringa.capitalize
puts stringa_capitalizzata # => "Ciao mondo"
```

Il metodo `.capitalize` di Ruby è comodo ma influisce solo sulla prima lettera. Per avere più controllo o per capitalizzare ogni parola in una stringa (noto come maiuscoletto), potresti voler usare il metodo `titleize` dall'estensione ActiveSupport di Rails, o implementarlo tu stesso:

```ruby
# Utilizzando 'titleize' di ActiveSupport in Rails
require 'active_support/core_ext/string/inflections'
stringa = "ciao mondo"
puts stringa.titleize # => "Ciao Mondo"
```

Se non stai utilizzando Rails o preferisci una soluzione puramente Ruby, ecco come potresti capitalizzare ogni parola in una stringa:

```ruby
stringa = "ciao mondo"
capitalizzata_ogni_parola = stringa.split.map(&:capitalize).join(' ')
puts capitalizzata_ogni_parola # => "Ciao Mondo"
```

Questo metodo divide la stringa in un array di parole, capitalizza ognuna di esse, quindi le unisce nuovamente insieme con uno spazio.
