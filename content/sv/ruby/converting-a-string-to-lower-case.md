---
title:    "Ruby: Omvandla en sträng till gemener"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Konvertering av en sträng till gemener (lower case) är ett vanligt förekommande problem i programmering. Ofta behöver vi hantera data som innehåller både stora och små bokstäver, men ibland vill vi enkelt göra om allt till gemener för att underlätta sökning, filtrering eller jämförelse. I detta blogginlägg kommer vi att gå igenom hur du kan göra detta i Ruby.

## Hur man gör

För att konvertera en sträng till gemener i Ruby kan du använda metoden `downcase`. Här är ett exempel på hur en sträng kan se ut före och efter konvertering:

```ruby
sträng = "HeLLo wOrLD"
puts sträng.downcase

# Output:
hello world
```

Som du kan se har alla bokstäver i strängen blivit omvandlade till gemener. Det är så enkelt det är att konvertera en sträng till gemener i Ruby!

## Djupdykning

Att konvertera en sträng till gemener är vanligtvis inte något som kräver en djupdykning i programmering, men det finns ändå några intressanta detaljer kring detta ämne. Till exempel kan du använda `downcase` på alla slags strängar, oavsett om de är i Unicode-form eller inte. Detta innebär att du kan hantera olika typer av tecken utan problem.

Ytterligare en detalj att tänka på är att `downcase` endast påverkar bokstäver, och inte andra tecken som kan förekomma i en sträng. Så om du har annan data i din sträng, som siffror eller specialtecken, kommer de inte att påverkas av metoden.

Det är också värt att notera att det finns en motsvarande metod, `upcase`, som omvandlar en sträng till versaler istället för gemener.

## Se även

Om du vill lära dig mer om strängar och textmanipulation i Ruby, kan följande resurser vara till hjälp:

- [Ruby Strings documentation](https://www.ruby-lang.org/en/documentation/stdlib/string/)
- [Ruby String manipulation tutorial](https://www.rubyguides.com/2019/02/ruby-string-methods/)
- [Ruby Guides: Working with Unicode strings](https://www.rubyguides.com/2015/05/ruby-strings/)