---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "capitalizing a string” innebär att göra den första bokstaven i en textsträng stor (eller versal). Programmerare gör detta för att följa språkliga normer, förbättra läsbarheten och hantera användarinmatningar.

## Så här gör du:
```Ruby
# Exempel 1: Använda `capitalize`
greeting = "hej värld"
puts greeting.capitalize  # Output: "Hej värld"

# Exempel 2: Capitalize alla ord
phrase = "välkommen till ruby"
puts phrase.split.map(&:capitalize).join(' ')  # Output: "Välkommen Till Ruby"

# Exempel 3: Capitalize med 'unicode-utils'
require 'unicode_utils/upcase'
puts UnicodeUtils.upcase("köttbullar är gott", :sv)  # Output: "Köttbullar Är Gott"
```

## Fördjupning
Att göra bokstäver stora är inte något nytt. Historiskt har vi använt detta i skriftspråket för att markera namn och början på meningar. I Ruby kan `.capitalize` göra första bokstaven stor, men bara den. För varierande behov och andra språk än engelska kan metoden vara begränsad, eftersom den inte hanterar speciella tecken eller fler ord. 

Alternativ inkluderar `.titleize` från Rails som stora bokstäver på alla ord i en sträng, eller externa bibliotek som 'unicode-utils' som hanterar internationella tecken på ett mer fullständigt sätt. I utförandet påpekar vi att `capitalize` endast gör det första tecknet stort och omvandlar resten av strängen till små bokstäver, medan `upcase` från 'unicode-utils' kan stora bokstäver på alla tecken i enlighet med Unicode-standarder.

## Se även
- Ruby-dokumentationen: https://ruby-doc.org/core-3.1.2/String.html#method-i-capitalize
- Rails-guide för `titleize`: https://api.rubyonrails.org/classes/String.html#method-i-titleize
- Unicode Utils gem: https://github.com/lang/unicode_utils
- Ruby Style Guide: https://rubystyle.guide/#strings
