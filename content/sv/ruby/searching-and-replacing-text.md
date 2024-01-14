---
title:                "Ruby: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift inom programmering. Oavsett om du behöver täcka upp känsliga uppgifter i en textsträng eller byta ut föråldrade variabler, är det ett användbart verktyg för att effektivisera och förbättra din kod.

## Hur man gör

För att söka och ersätta text med hjälp av Ruby, kan du använda `gsub`-metoden. Den tar två argument: det första är det du vill söka efter och det andra är vad du vill ersätta det med. Här är ett exempel på kod:

```Ruby
text = "Jag älskar att koda med Ruby!"

text.gsub("Ruby", "Python") 

=> "Jag älskar att koda med Python!"
```

Som vi ser i exemplet ovan, ersätts "Ruby" med "Python". Om du vill söka och ersätta flera förekomster av en sträng, kan du ange ett tredje argument som anger hur många gånger `gsub`-metoden ska utföras. Här är ett exempel:

```Ruby
text = "Katt, katt, hund, katt, kanin"

text.gsub("katt", "gris", 2) 

=> "Gris, gris, hund, katt, kanin"
```

Det tredje argumentet (2 i det här fallet) gör att metoden bara kommer att ersätta de två första förekomsterna av "katt" med "gris".

## Djupdykning

Om du vill vara mer specifik i din sökning och ersättning, kan du använda en så kallad "regular expression" (reguljärt uttryck) i stället för en enkel sträng. Detta ger dig mer kontroll över vad du letar efter och vad du vill ersätta det med. Här är ett exempel på hur du kan använda en reguljär expressoin i `gsub`-metoden:

```Ruby
text = "Min lösenord är 1234"

text.gsub(/\d/, "X") 

=> "Min lösenord är XXXX"
```

I exemplet ovan letar `gsub`-metoden efter alla siffror och ersätter dem med ett "X". Det finns många olika mönster och uttryck som du kan använda i reguljära uttryck, så det kan vara värt att utforska mer om du vill bli mer avancerad i din kodning.

## Se också

Här är några användbara resurser för vidare läsning och övning:

- [Ruby Dokumentation för `gsub`-metoden](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub)
- [Regular Expressions i Ruby](https://www.rubyguides.com/2015/06/ruby-regular-expressions/)
- [Regex101 - ett verktyg för att testa reguljära uttryck på nätet](https://regex101.com/).