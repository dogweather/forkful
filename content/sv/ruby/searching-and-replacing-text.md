---
title:    "Ruby: Sökning och ersättning av text"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Ibland när vi arbetar med programmering, stöter vi på text som behöver ändras eller ersättas. Det kan vara ett namn, en länk eller annan information som behöver uppdateras. Att lära sig hur man söker och ersätter text är en viktig färdighet för att effektivt kunna redigera kod.

## Så här gör du
Det finns flera olika sätt att söka och ersätta text i Ruby, men det vanligaste är att använda sig av den inbyggda `gsub`-metoden. Här är ett exempel på hur du skulle kunna använda den:

```ruby
str = "Hej världen!"
puts str.gsub("hej", "Hallå")
=> Hallå världen!
```

I detta exempel ersätter vi ordet "hej" med "Hallå" i strängen "Hej världen!". Det är viktigt att notera att `gsub`-metoden är casesensitiv, vilket betyder att den bara kommer att ersätta text som matchar exakt.

Du kan också använda en RegEx (regular expression) med `gsub` för att söka efter mer specifik text. Till exempel:

```ruby
str = "Jag gillar äpplen men inte bananer"
puts str.gsub(/äpplen|bananer/, "frukter")
=> Jag gillar frukter men inte frukter
```

Här använder vi `|` för att matcha både "äpplen" och "bananer" och ersätta dem med "frukter". Detta sparar oss tid och gör det möjligt att söka efter flera kombinationer av ord på samma gång.

## Djupdykning
Förutom `gsub`-metoden finns det andra sätt att söka och ersätta text i Ruby. En annan vanlig metod är `sub`, som bara ersätter första matchningen. Det finns också möjlighet att använda `tr`-metoden för att byta ut specifika tecken i en sträng.

Oavsett vilken metod du väljer att använda, är det viktigt att förstå skillnaderna och begränsningarna hos varje metod.

## Se även
- [Ruby's gsub-metod](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Om Regular Expressions (på svenska)](https://rot13.glitch.me/regex)
- [10 saker som alla borde veta om Ruby Regex](https://medium.com/@TheGallahad/10-things-every-ruby-programmer-should-know-about-regular-expressions-263f8b4811f1)