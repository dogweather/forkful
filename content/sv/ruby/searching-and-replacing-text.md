---
title:    "Ruby: Söka och ersätta text"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför
 
Att söka och ersätta text är en vanlig uppgift inom programmering, särskilt när man arbetar med stora mängder text eller ständigt föränderliga databaser. Genom att lära sig söka och ersätta text effektivt kan du spara tid och göra ditt kodande mer effektivt.
 
## Hur man gör
 
För att söka och ersätta text i Ruby finns det flera olika verktyg och metoder. Ett enkelt sätt är att använda strängmetoder som `gsub` för att ersätta en viss del av en sträng med en annan del. Till exempel kan du använda `gsub` för att ersätta alla förekomster av ett visst ord med ett annat ord i en textsträng.
 
```Ruby
sträng = "Hej! Välkommen till min blogg!"
sträng.gsub!(/Hej/, "Hallå")
 
puts sträng
# Output: Hallå! Välkommen till min blogg!
```
 
Du kan också använda metoder som `scan` och `match` för att söka efter specifika mönster i en text och sedan välja ut och ersätta delar av texten baserat på det.
 
```Ruby
sträng = "Max, Mia, Johan och Lisa är vänner."
 
sträng.scan(/(Max|Mia|Johan|Lisa)/) do |match|
  puts "Bra jobbat #{match}, ni är en härlig grupp av vänner!"
end
 
# Output: Bra jobbat Max, ni är en härlig grupp av vänner!
# Bra jobbat Mia, ni är en härlig grupp av vänner!
# Bra jobbat Johan, ni är en härlig grupp av vänner!
# Bra jobbat Lisa, ni är en härlig grupp av vänner!
```
 
## Djupdykning
 
Det finns många olika metoder och tekniker för att söka och ersätta text i Ruby, och det kan vara värt att lära sig fler av dem för att bli en mer effektiv programmerare. Till exempel kan du använda Regular Expressions för att söka efter mer komplexa mönster i en text, och sedan använda backreferences för att ersätta delar av texten på ett mer dynamiskt sätt.
 
## Se även
 
Här är några länkar till resurser som kan hjälpa dig att lära dig mer om att söka och ersätta text i Ruby:
 
- [Ruby Dokumentation för String-klassen](https://ruby-doc.org/core-2.7.0/String.html)
- [En "hands-on" guide till Regular Expressions i Ruby](https://medium.com/@jcrossling/regular-expressions-in-ruby-hands-on-tutorial-727a0e3cac03)
- [Rubular - en online regex-testare för Ruby](https://rubular.com/)