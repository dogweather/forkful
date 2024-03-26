---
title:                "Gör om en sträng till versaler"
date:                  2024-03-25T17:32:00.737598-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva med stor bokstav innebär vanligtvis att konvertera det första tecknet i en sträng till versal och resten till gemen. Men ibland kan det innebära att bara se till att det första tecknet är en versal medan resten av strängen förblir oförändrad. Ärligt talat, enligt min åsikt, är det en något vag term.

## Hur gör man:
Ruby erbjuder [enkla metoder för strängmanipulation](https://docs.ruby-lang.org/en/3.3/String.html), inklusive att göra stor bokstav:

```ruby
# Rubys inbyggda metod
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Mycket praktiskt.

Rubys `.capitalize`-metod är bekväm men gör bara första bokstaven stor. För mer kontroll eller för att skriva varje ord i en sträng med stor bokstav (känt som titelform), kanske du vill använda `titleize`-metoden från Rails ActiveSupport-tillägget, eller implementera den själv:

```ruby
# Använda ActiveSupports 'titleize' i Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# En egen lösning
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Denna metod delar strängen till en array av ord, gör varje ord med stor bokstav och sedan fogar ihop dem igen med ett mellanrum.

Personligen tar jag denna idé mycket längre i min kod. Jag skrev min egen [`titleize`-metod som tar hänsyn till små ord som "a" och "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
