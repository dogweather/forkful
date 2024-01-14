---
title:                "Ruby: Kapitalisering av en sträng"
simple_title:         "Kapitalisering av en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna omvandla strängar till versaler är en viktig del av Ruby-programmering. Det kan hjälpa till att förbättra användbarheten och presentationen av ditt program och göra det mer lättläst. Du kan också använda det för att lösa specifika problem med strängmanipulering. 

## Såhär går det till

För att omvandla en sträng till versaler, använder vi metoden `upcase`. Det är en inbyggd metod i Ruby som enkelt konverterar alla bokstäver i en sträng till versaler. Låt oss ta en titt på ett enkelt exempel:

```Ruby
strang = "hej allihopa!"

puts strang.upcase
```

Output: HEJ ALLIHOPA!

Som du kan se så omvandlades alla bokstäver i strängen till versaler. Nu kanske du undrar vad som händer med å, ä och ö. Detta beror på att den `upcase` metoden bara gäller för bokstäver inom det engelska alfabetet. Om du vill att även å, ä och ö ska omvandlas till versaler, kan du använda metoden `mb_upcase`, vilket hanterar unicode-tecken.

```Ruby
strang = "hej på dej!"

puts strang.mb_upcase
```
Output: HEJ PÅ DEJ!

Som du ser så behåller `mb_upcase` även å, ä och ö i sin omvandling.

## Djupdykning

För att förstå varför `upcase` inte fungerar för å, ä, och ö inom det engelska alfabetet, behöver vi förstå skillnaden mellan en byte och ett unicode-tecken. Ett byte är en åtta-bitars enhet som används för att representera en enda symbol i ett teckenkodningssystem. Å andra sidan är unicode-tecken en universal standard som möjliggör representation av alla möjliga skrifttecken. För att hantera å, ä, och ö, som är unicode-tecken, behöver vi använda den speciella metoden `mb_upcase` för att säkerställa att de också konverteras till versaler.

För att förstå mer om teckenkodning och dess betydelse inom Ruby-programmering, kan du läsa mer här [1][teckenkodning-wikipedia] och här [2][ruby-teckenkodning].

## Se även

Här hittar du mer information om teckenkodning och andra användbara Ruby-metoder.

[teckenkodning-wikipedia]: https://sv.wikipedia.org/wiki/Teckenkodning
[ruby-teckenkodning]: https://www.ruby-lang.org/sv/documentation/faq/8/#encoding