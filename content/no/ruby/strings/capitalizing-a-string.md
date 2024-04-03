---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: 'Hvordan: Ruby tilbyr [enkle metoder for manipulasjon av strenger](https://docs.ruby-lang.org/en/3.3/String.html),
  inkludert kapitalisering.'
lastmod: '2024-03-25T19:22:00.718759-06:00'
model: gpt-4-0125-preview
summary: Ruby tilbyr [enkle metoder for manipulasjon av strenger](https://docs.ruby-lang.org/en/3.3/String.html),
  inkludert kapitalisering.
title: Sette store bokstaver i en streng
weight: 2
---

## Hvordan:
Ruby tilbyr [enkle metoder for manipulasjon av strenger](https://docs.ruby-lang.org/en/3.3/String.html), inkludert kapitalisering:

```ruby
# Rubys innebygde metode
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Veldig praktisk.

Rubys `.capitalize` metode er praktisk, men den gjør bare det første brevet stort. For mer kontroll eller for å sette stor bokstav på hvert ord i en streng (kjent som tittelkasus), kan det hende du vil bruke `titleize` metoden fra Rails ActiveSupport-utvidelsen, eller implementere den selv:

```ruby
# Bruker ActiveSupports 'titleize' i Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# En hjemmelaget løsning
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Denne metoden deler strengen inn i et ordnett, kapitaliserer hvert ord, og så setter dem sammen igjen med et mellomrom.

Personlig tar jeg denne ideen mye lenger i koden min. Jeg skrev min egen [`titleize` metode som tar hensyn til små ord som "en" og "og"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
