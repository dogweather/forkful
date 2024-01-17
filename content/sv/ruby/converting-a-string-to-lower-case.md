---
title:                "Konvertera en sträng till gemener"
html_title:           "Ruby: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng (text) till små bokstäver är en vanlig åtgärd inom programmering. Detta används för att jämföra strängar oberoende av storleken på deras bokstäver, eller för att formatera en text på ett enhetligt sätt. Till exempel kan en användare skriva in "Hej" eller "hej" och båda ska vara giltiga input.

## Hur man gör:
```Ruby
"HELLO WORLD".downcase
# => "hello world"
```
```Ruby
"Ruby Programming".downcase
# => "ruby programming"
```
```Ruby
"Jag ÄLSKAR Ruby".downcase
# => "jag älskar ruby"
```

## Djupdykning:
Att konvertera en sträng till små bokstäver är inte en ny idé, det har funnits sedan början av programmeringens dagar. Det finns också alternativ till .downcase-metoden, som till exempel .capitalize (konverterar första bokstaven till stor bokstav) och .upcase (konverterar alla bokstäver till stora). Det är också viktigt att notera att vissa språk hanterar bokstäver på olika sätt, och programmerare måste se till att de använder rätt metod för det specifika språket de arbetar med.

## Se också:
- [Ruby documentation on string manipulation](https://ruby-doc.org/core-3.0.0/String.html#method-i-downcase)
- [Strings in programming](https://www.digitalocean.com/community/tutorials/5-String-Manipulation-Methods-in-Ruby)
- [Other string manipulation methods in Ruby](https://ruby-doc.org/core-3.0.0/String.html)