---
title:                "Ruby: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför 
Att radera tecken som matchar ett mönster kan vara användbart i många situationer. Det kan hjälpa till att städa upp data eller formatera strängar på ett önskvärt sätt. Det är också ett bra sätt att lära sig mer om reguljära uttryck och hur de fungerar i Ruby.

## Såhär gör du 
Den enklaste metoden för att radera tecken som matchar ett mönster är att använda `gsub` metoden. Här är en kodexempel på hur du kan använda det för att ta bort alla specialtecken från en sträng:
```Ruby
str = "Hello!# World%$!"
str.gsub!(/[^\w\s]/, "")
puts str # Output: Hello World
```

I det här exemplet använder vi en reguljärt uttryck för att matcha alla tecken som inte är antingen bokstäver, siffror eller mellanslag. Sedan ersätter vi dem med en tom sträng, vilket tar bort dem från den ursprungliga strängen. Det här är bara ett enkelt exempel, men genom att experimentera med olika reguljära uttryck kan du uppnå olika resultat.

## Djupdykning 
För att förstå mer om hur detta fungerar, låt oss titta på en mer avancerad kod här:
```Ruby
str = "Apple, banana, orange"
str.gsub!(/(apple|banana)/i, "fruit")
puts str # Output: fruit, fruit, orange
```

Här använder vi en reguljärt uttryck med en alternativ grupp för att matcha både "apple" och "banana" oavsett om de är stavade med stor eller liten bokstav. Detta gör att vi kan ersätta båda orden med "fruit" utan att behöva skapa två separata uttryck.

## Se även 
- [Reguljära uttryck i Ruby](https://www.rubyguides.com/regex-ruby/)
- [Dokumentation för `gsub` metoden](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)