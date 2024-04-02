---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Att versalisera en str\xE4ng betyder vanligtvis att omvandla det f\xF6\
  rsta tecknet i en str\xE4ng till versal (stort bokstav) och resten till gemener\
  \ (sm\xE5\u2026"
lastmod: '2024-03-25T19:21:58.553462-06:00'
model: gpt-4-0125-preview
summary: "Att versalisera en str\xE4ng betyder vanligtvis att omvandla det f\xF6rsta\
  \ tecknet i en str\xE4ng till versal (stort bokstav) och resten till gemener (sm\xE5\
  \u2026"
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Vad & Varför?
Att versalisera en sträng betyder vanligtvis att omvandla det första tecknet i en sträng till versal (stort bokstav) och resten till gemener (små bokstäver). Men ibland kan det innebära att endast försäkra sig om att det första tecknet är en versal medan resten av strängen förblir oförändrad. Ärligt talat, i min åsikt, är det en ganska vag term.

## Hur:
Ruby tillhandahåller [rakvägade metoder för strängmanipulering](https://docs.ruby-lang.org/en/3.3/String.html), inklusive kapitalisering:

```ruby
# Rubys inbyggda metod
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Mycket praktiskt.

Rubys `.capitalize` metod är bekväm, men gör bara det första bokstaven stor. För mer kontroll eller för att kapitalisera varje ord i en sträng (känt som titel-fall), kanske du vill använda `titleize` metoden från Rails ActiveSupport-tillägget, eller implementera det själv:

```ruby
# Använda ActiveSupports 'titleize' i Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# En hemmagjord lösning
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Denna metod delar upp strängen i en array av ord, gör varje ett av dem stort, sedan sammanfogar dem återigen med ett mellanrum.

Personligen tar jag denna idé mycket längre i min kod. Jag skrev min egen [`titleize` metod som tar hänsyn till små ord som "a" och "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
