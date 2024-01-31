---
title:                "Utilizzo delle espressioni regolari"
date:                  2024-01-19
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Le espressioni regolari (regex) sono strumenti che consentono di cercare pattern di testo con grande flessibilità. I programmatori le usano per validare, estrarre e manipolare dati in maniera efficiente e precisa.

## How to:
```Ruby
# Trovare se una stringa contiene un numero
contiene_numero = /\d/.match?("Hai 2 messaggi")
puts contiene_numero  # => true

# Estrarre tutti i numeri da una stringa
numeri = "L'anno è 2023 e siamo in aprile".scan(/\d+/)
puts numeri.join(', ')  # => "2023, 4"

# Sostituire testo in una stringa
stringa_modificata = "ruby è divertente".gsub(/divertente/, 'fantastico')
puts stringa_modificata  # => "ruby è fantastico"
```

## Deep Dive
Le espressioni regolari sono nate negli anni '50 con automi e teoria degli insiemi. Alternatives include string parsing libraries or specific parser for structured data like JSON or XML. In Ruby, le regex sono implementate con la libreria Onigmo, che supporta diversi flussi e codifiche di caratteri.

## See Also
- [Ruby Regular Expressions](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Rubular: a Ruby regular expression editor](http://rubular.com/)
- [Ruby Learning - Regular Expressions](http://rubylearning.com/satishtalim/ruby_regular_expressions.html)
