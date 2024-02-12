---
title:                "Sette stor bokstav i en streng"
aliases:
- /no/ruby/capitalizing-a-string/
date:                  2024-02-03T19:06:11.636584-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sette stor bokstav i en streng"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng i programmering refererer ofte til å konvertere det første tegnet i en streng til stor bokstav og resten til små bokstaver. Programmerere gjør dette av grunner som å følge navnekonvensjoner, gjøre utdata mer lesbare, eller sikre datakonsistens for sammenligninger og lagring.

## Hvordan:
Ruby tilbyr enkle metoder for manipulasjon av strenger, inkludert kapitalisering. Her er hvordan du kan kapitalisere en streng i Ruby:

```ruby
# Rubys innebygde metode
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Rubys `.capitalize` metode er praktisk, men påvirker kun det første bokstaven. For mer kontroll eller for å kapitalisere hvert ord i en streng (kjent som tittelkasus), vil du kanskje bruke `titleize` metoden fra Rails ActiveSupport-utvidelsen, eller implementere den selv:

```ruby
# Bruker ActiveSupports 'titleize' i Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Hvis du ikke bruker Rails eller foretrekker en ren Ruby-løsning, her er hvordan du kan kapitalisere hvert ord i en streng:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Denne metoden splitter strengen inn i et array av ord, kapitaliserer hver enkelt, og deretter setter dem sammen igjen med et mellomrom.
