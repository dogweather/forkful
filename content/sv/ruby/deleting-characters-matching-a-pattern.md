---
title:                "Ruby: Borttagning av tecken som matchar ett mönster"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Ibland när du arbetar med textsträngar i Ruby kanske du vill ta bort vissa karaktärer som matchar ett mönster. Detta kan vara användbart för att rensa en text från oönskade tecken eller för att formatera strängar på ett specifikt sätt.

## Så här gör du

För att ta bort karaktärer som matchar ett visst mönster från en sträng i Ruby, kan du använda metoden `gsub!()`. Denna metod tar två argument - mönstret som du vill matcha och ersättningsvärdet för de matchande karaktärerna. Nedan är ett exempel på hur man skulle använda denna metod för att ta bort alla siffror från en sträng:

```Ruby
sträng = "Det finns 123 äpplen i korgen."
sträng.gsub!(/\d+/, "")
puts sträng # Output: "Det finns  äpplen i korgen."
```

I exemplet ovan använde vi det reguljära uttrycket `/ \ d + /` för att matcha alla förekomster av siffror i strängen. Sedan ersatte vi dem med en tom sträng, vilket resulterade i att siffrorna togs bort från den ursprungliga strängen.

`gsub!()`-metoden kan också användas för att ta bort andra karaktärer, till exempel skiljetecken eller andra symboler. Det viktiga är att det mönster som används matchar de karaktärer som du vill ta bort.

## Djupdykning

För att förstå bättre hur `gsub!()`-metoden fungerar, låt oss titta på en mer detaljerad förklaring. För det första, vad betyder `gsub`? Det står för "global substitute", vilket betyder att det är en global ersättning för alla matchande mönster i strängen.

När det gäller `gsub!()`-metoden, är utropstecknet viktigt eftersom det innebär att den kommer att ändra den ursprungliga strängen istället för att bara returnera en kopia av den. Detta är användbart om du vill göra en permanent förändring i en sträng, istället för att bara ändra den tillfälligt.

En annan viktig sak att notera är att `gsub!()`-metoden är en del av `String`-klassen i Ruby. Det betyder att du kan använda den på alla olika typer av strängar, inklusive strängar som du har skapat själv eller strängar som returneras av andra metoder.

## Se också

- [Reguljära uttryck i Ruby](https://www.rubyguides.com/ruby-tutorial/regexp/)
- [Radering av strängar i Ruby](https://www.geeksforgeeks.org/ruby-string-gsub-method-with-examples/)
- [Ruby String-klassen](https://ruby-doc.org/core-2.7.3/String.html)