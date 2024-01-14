---
title:                "Ruby: Att hitta längden av en sträng"
simple_title:         "Att hitta längden av en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför?

Att hitta längden på en sträng är en vanlig och användbar uppgift i Ruby-programmering. Det kan hjälpa dig att hantera data och manipulera text på ett effektivt sätt. Dessutom kan det vara en grundläggande färdighet som kan hjälpa dig att förstå mer komplexa koncept inom programmering.

## Så här gör man

För att hitta längden på en sträng kan du använda dig av metoden `length`, som finns tillgänglig för alla strängar i Ruby. Detta ger dig antalet tecken i strängen, inklusive mellanslag och specialtecken. Se nedan för ett exempel:

```Ruby
sträng = "Hej, detta är en sträng!"
puts sträng.length
```

Output:
```Ruby
24
```

## Deep Dive

Det finns flera sätt att hitta längden på en sträng i Ruby. En annan metod är `size`, som fungerar på samma sätt som `length`. Det finns också metoden `bytesize`, som ger dig längden på strängen i bytes istället för tecken.

För att hitta längden på en sträng utan att räkna med mellanslag och specialtecken, kan du använda metoden `strip` först, som tar bort mellanslag från början och slutet av strängen. Sedan kan du använda `length` för att räkna antalet tecken.

Det är också viktigt att vara medveten om att teckenkodning (encoding) kan påverka längden på en sträng. Om du arbetar med icke-latiniska tecken eller specialtecken, se till att din sträng är korrekt kodad för att få en korrekt längd.

## Se även

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby String Methods](https://www.rubyguides.com/2019/02/ruby-string-methods/)
- [Understanding Strings in Ruby](https://hackhands.com/ruby-string-exercises/)