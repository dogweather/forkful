---
title:                "Een String Met Hoofdletters Maken"
date:                  2024-03-25T17:32:00.157324-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een string met hoofdletters schrijven betekent meestal het omzetten van het eerste karakter van een string naar een hoofdletter en de rest naar kleine letters. Maar soms kan het betekenen dat je ervoor zorgt dat alleen het eerste karakter een hoofdletter is, terwijl de rest van de string ongewijzigd blijft. Eerlijk gezegd, naar mijn mening, is het een enigszins vaag begrip.

## Hoe:
Ruby biedt [eenvoudige methoden voor stringmanipulatie](https://docs.ruby-lang.org/en/3.3/String.html), inclusief het gebruik van hoofdletters:

```ruby
# Ruby's ingebouwde methode
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Erg handig.

Ruby's `.capitalize` methode is handig, maar zet alleen de eerste letter in een hoofdletter. Voor meer controle of om elk woord in een string met een hoofdletter te schrijven (bekend als titelcase), wil je misschien de `titleize` methode uit de Rails ActiveSupport-extensie gebruiken, of het zelf implementeren:

```ruby
# 'titleize' gebruiken in Rails van ActiveSupport
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Een zelfgemaakte oplossing
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Deze methode splitst de string in een array van woorden, zet elk ervan in hoofdletters, en voegt ze dan weer samen met een spatie.

Persoonlijk neem ik dit idee veel verder in mijn code. Ik schreef mijn eigen [`titleize` methode die rekening houdt met kleine woorden zoals "a" en "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
