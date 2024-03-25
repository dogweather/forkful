---
title:                "Sette stor bokstav i en streng"
date:                  2024-03-25T17:31:51.052975-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å sette stor bokstav i en streng betyr vanligvis å konvertere det første tegnet i en streng til store bokstaver og resten til små bokstaver. Men noen ganger kan det også bety bare å sørge for at det første tegnet er med store bokstaver mens resten av strengen forblir uendret. For å være ærlig, etter min mening, er det et noe vagt begrep.

## Hvordan:
Ruby tilbyr [enkle metoder for manipulering av strenger](https://docs.ruby-lang.org/en/3.3/String.html), inkludert stor skrivemåte:

```ruby
# Ruby's innebygde metode
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Veldig praktisk.

Ruby sin `.capitalize` metode er bekvem, men gjør kun det første bokstavet stort. For mer kontroll eller for å sette stor bokstav på hvert ord i en streng (kjent som tittelskrivemåte), kan du ønske å bruke `titleize`-metoden fra Rails ActiveSupport-utvidelsen, eller implementere den selv:

```ruby
# Bruk av ActiveSupport sin 'titleize' i Rails
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

Denne metoden deler strengen inn i et ordarré, setter stor bokstav i hvert ord, og deretter slår dem sammen igjen med et mellomrom.

Personlig tar jeg denne ideen mye lenger i min kode. Jeg skrev min egen [`titleize`-metode som tar hensyn til små ord som "a" og "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
