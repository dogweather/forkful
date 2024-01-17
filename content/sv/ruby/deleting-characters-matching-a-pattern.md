---
title:                "Radera tecken som matchar ett mönster"
html_title:           "Ruby: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken som matchar ett mönster är en användbar funktion inom programmering. Det innebär att vi kan välja vilka tecken som ska tas bort från en sträng baserat på ett specifikt mönster. Detta sparar tid och gör koden mer effektiv.

## Hur man:
För att ta bort tecken som matchar ett mönster i Ruby, använder vi metoden `.gsub!` tillsammans med en regex (regular expression) för att söka efter mönstret. Här är ett exempel som tar bort alla siffror från en sträng:

```Ruby
str = "ABC123def456"
str.gsub!(/\d/, "")
puts str #=> ABCdef
```

Här kan vi se att alla siffror (1, 2, 3, 4 och 5) har tagits bort från strängen och endast bokstäverna kvarstår. Vi kan också specificera vilka tecken vi vill ta bort genom att ändra regex-mönstret.

## Djupdykning:
Att ta bort tecken som matchar ett mönster är en viktig del av regex-programmering, då det möjliggör mer avancerad manipulation av strängar. Regex används ofta för att hitta och ersätta specifika delar av texter eller för att strukturera data på ett snabbare sätt. Alternativt kan metoden `.delete` i Ruby användas för att ta bort tecken baserat på ett visst mönster.

När mönstret matchar flera tecken kan det vara fördelaktigt att använda `.gsub!` istället för `.sub!`, eftersom `.gsub!` kommer ta bort alla förekomster av mönstret medan `.sub!` bara tar bort den första förekomsten.

## Se också:
För mer information om regex och dess användning i Ruby, se följande länkar:

- [The Ruby Programming Language by Yukihiro Matsumoto](https://www.amazon.com/Ruby-Programming-Language-Yukihiro-Matsumoto/dp/0596516177)
- [RubyMonk - Regular Expressions](https://rubymonk.com/learning/books/1-ruby-primer/problems/158-validation-using-regular-expressions)
- [Ruby Doc - Regexp](https://ruby-doc.org/core-2.7.0/Regexp.html)
- [Ruby Doc - String](https://ruby-doc.org/core-2.7.0/String.html)