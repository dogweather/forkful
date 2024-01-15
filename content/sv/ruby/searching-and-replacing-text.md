---
title:                "Sökning och ersättning av text"
html_title:           "Ruby: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Ibland behöver man göra stora ändringar i en text eller kod för att uppnå ett specifikt resultat. Istället för att manuellt gå igenom och ändra varje instans av en viss term kan man använda sig av sök- och ersättningsfunktionen i Ruby för att göra detta snabbt och effektivt.

## Hur man gör

För att söka och ersätta text i en sträng kan man använda sig av metoden `gsub` (global substitution) tillsammans med ett reguljärt uttryck. Detta är ett sätt att matcha och ersätta text baserat på ett mönster.

Ett enkelt exempel är att byta ut alla förekomster av ordet "hej" med ordet "tjena" i en sträng:

```Ruby
str = "Hej världen!"
ny_str = str.gsub(/hej/, "tjena")
puts ny_str 
# Output: "Tjena världen!"
```

Om man vill vara mer specifik kan man ange en modifierare efter uttrycket för att definiera sökningen. Till exempel, om vi bara vill byta ut ordet "hej" om det förekommer i början av en mening, kan vi använda modifieraren `^` som betyder "börja på":

```Ruby
str = "Hej världen! Hej igen!"
ny_str = str.gsub(/^hej/, "tjena")
puts ny_str 
# Output: "Tjena världen! Hej igen!"
```

Man kan också ersätta mer än en term samtidigt genom att använda en normal array med sök- och ersättningsord som argument:

```Ruby
str = "Hej världen! Hej igen!"
ny_str = str.gsub(["hej", "igen"], ["tjena", "en sista gång"])
puts ny_str 
# Output: "Tjena världen! En sista gång igen!"
```

## På djupet

Det finns många olika sätt att använda sök- och ersättning i Ruby, inklusive att matcha och ersätta baserat på mönster och regler. Man kan också använda sig av block-funktionen för att ändra text baserat på specifika villkor eller logik. Det finns många resurser där man kan lära sig mer om detta ämne, inklusive Ruby-dokumentationen och olika forum och communitys på nätet.

## Se även

- [Ruby-dokumentation: Strängar](https://ruby-doc.org/core-3.0.1/String.html)
- [Ruby for Beginners: Finding and Replacing Text](https://ruby-for-beginners.rubymonstas.org/fundamentals/strings/finding-and-replacing-text.html)
- [Stack Overflow: How to replace text in a Ruby string](https://stackoverflow.com/questions/6568053/how-to-replace-text-in-a-ruby-string)