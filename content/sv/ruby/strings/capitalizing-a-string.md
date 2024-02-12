---
title:                "Gör om en sträng till versaler"
aliases:
- sv/ruby/capitalizing-a-string.md
date:                  2024-02-03T19:06:15.605992-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva med stor bokstav i programmering hänvisar ofta till att konvertera det första tecknet i en sträng till versal och resten till gemener. Programmerare gör detta av skäl som att följa namnkonventioner, göra utdata mer läsliga eller säkerställa datakonsistens för jämförelser och lagring.

## Hur:
Ruby erbjuder enkla metoder för strängmanipulering, inklusive att sätta stor bokstav. Så här kan du skriva med stor bokstav i en sträng i Ruby:

```ruby
# Rubys inbyggda metod
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Rubys metod `.capitalize` är bekväm, men påverkar bara den första bokstaven. För mer kontroll eller för att skriva varje ord i en sträng med stor bokstav (känt som titelstorlek), kanske du vill använda metoden `titleize` från Rails ActiveSupport-tillägg, eller implementera den själv:

```ruby
# Använder ActiveSupports 'titleize' i Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Om du inte använder Rails eller föredrar en ren Ruby-lösning, här är hur du kan skriva varje ord i en sträng med stor bokstav:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Denna metod delar upp strängen i en ordlista, skriver varje ord med stor bokstav och sätter sedan ihop dem igen med ett mellanslag.
