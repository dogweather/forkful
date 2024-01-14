---
title:    "Ruby: Extrahera delsträngar"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Varför
I Ruby-programmering finns det ofta behov av att extrahera delsträngar från en given sträng. Det kan vara för att bearbeta data, filtrera information eller helt enkelt för att göra manipulationer på text. Oavsett syfte är det användbart att kunna extrahera substränger på ett effektivt sätt.

## Hur man gör
Att extrahera substränger är en relativt enkel process i Ruby. Det finns flera metoder som kan användas för detta ändamål, men en av de vanligaste är `.slice()`. Nedan följer ett exempel på hur man kan använda denna metod för att extrahera en delsträng från en längre text.

```Ruby
text = "Hej, jag heter Lisa och jag älskar att programmera."
delsträng = text.slice(13..17)

puts delsträng
# resultat: Lisa
``` 

I detta exempel använder vi `.slice()` för att extrahera delsträngen "Lisa" från texten. Vi tilldelar sedan resultatet till en ny variabel som vi sedan skriver ut.

## Djupdykning
Utöver `.slice()` finns det flera andra metoder som kan användas för att extrahera substränger. En annan vanlig metod är `.substring()` som fungerar på liknande sätt som `.slice()`. Ett viktigt att tänka på är att Ruby använder sig av en nollindexering vilket betyder att den första bokstaven i en sträng motsvaras av index 0. Detta kan ibland leda till förvirring när man använder sig av metoder som `.slice()` och `.substring()`, så det är viktigt att hålla koll på vilken bokstav som bara på index 0.

## Se även
- [Dokumentation för Ruby String-klassen](https://ruby-doc.org/core-2.6.3/String.html)
- [Tutorial för att jobba med strängar i Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Blogginlägg om Ruby substrings på engelska](https://www.skilledup.com/articles/extracting-substrings-ruby)