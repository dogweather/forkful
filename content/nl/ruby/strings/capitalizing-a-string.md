---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Het kapitaliseren van een string betekent meestal het omzetten van het\
  \ eerste karakter van een string naar een hoofdletter en de rest naar kleine letters.\u2026"
lastmod: '2024-03-25T19:22:02.456642-06:00'
model: gpt-4-0125-preview
summary: "Het kapitaliseren van een string betekent meestal het omzetten van het eerste\
  \ karakter van een string naar een hoofdletter en de rest naar kleine letters.\u2026"
title: Een string met hoofdletters maken
---

## Wat & Waarom?
Het kapitaliseren van een string betekent meestal het omzetten van het eerste karakter van een string naar een hoofdletter en de rest naar kleine letters. Maar soms kan het ook gewoon betekenen dat je ervoor zorgt dat het eerste karakter een hoofdletter is, terwijl je de rest van de string onveranderd laat. Eerlijk gezegd, naar mijn mening, is het een enigszins vaag begrip.

## Hoe:
Ruby biedt [eenvoudige methoden voor stringmanipulatie](https://docs.ruby-lang.org/en/3.3/String.html), inclusief kapitalisatie:

```ruby
# Ruby's ingebouwde methode
string = "hello WORLD"
gekapitaliseerde_string = string.capitalize
puts gekapitaliseerde_string # => "Hello world"
```

Heel handig.

Ruby's `.capitalize` methode is handig, maar maakt alleen de eerste letter een hoofdletter. Voor meer controle of om elk woord in een string te kapitaliseren (bekend als titelcase), wil je misschien de `titleize` methode gebruiken van de Rails ActiveSupport-extensie, of het zelf implementeren:

```ruby
# ActiveSupport's 'titleize' gebruiken in Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Een zelfgemaakte oplossing
string = "hello world"
elk_woord_gekapitaliseerd = string.split.map(&:capitalize).join(' ')
puts elk_woord_gekapitaliseerd # => "Hello World"
```

Deze methode splitst de string in een array van woorden, kapitaliseert elk woord en voegt ze vervolgens weer samen met een spatie.

Persoonlijk neem ik dit idee veel verder in mijn code. Ik heb mijn eigen [`titleize` methode geschreven die rekening houdt met kleine woorden zoals "a" en "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
