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

## Varför

Att kunna radera tecken som matchar ett visst mönster i en textsträng är en viktig del av Ruby-programmering. Detta gör det möjligt att rensa och manipulera data på ett effektivt sätt.

## Så här gör du

För att radera tecken som matchar ett visst mönster i en textsträng i Ruby använder vi metoden `.gsub!()` och specificerar det mönster vi vill matcha inom parenteserna. Detta är ett exempel på hur vi skulle ta bort alla siffror från en textsträng:

```Ruby
text = "Det har varit 5 dagar sedan jag senast skrev en text."
text.gsub!(/\d/,"")
puts text # Output: "Det har varit dagar sedan jag senast skrev en text."
```

Vi använder `\d` inuti parenteserna för att matcha alla siffror. Om vi istället vill ta bort alla vokaler från en textsträng, kan vi använda följande kod:

```Ruby
text = "Det här är en textsträng utan vokaler."
text.gsub!(/[aeiou]/,"")
puts text # Output: "Dt hr r n txtrng tn vklr."
```

Förutom att bara ta bort ett enskilt tecken kan vi också ta bort flera tecken genom att använda en range. Här är en kod som tar bort alla bokstäver mellan a och f från en textsträng:

```Ruby
text = "abcdefg"
text.gsub!(/[a-f]/,"")
puts text # Output: "g"
```

## Djupdykning

Som du kanske märkt används `/` runt mönstret i exempelkoden. Detta kallas för ett reguljärt uttryck (regular expression) och möjliggör mer avancerade sökningar än bara vanliga tecken. Det finns en mängd olika metatecken som kan användas för att matcha olika mönster, till exempel `.` för att matcha alla tecken och `+` för att matcha ett eller flera av samma tecken.

Det finns också flera modifierare som vi kan använda för att ändra hur matchningen fungerar, till exempel `i` för att ignorera skillnader i versaler och gemener och `m` för att göra det möjligt att matcha flera rader av en textsträng.

Att ha en grundläggande förståelse för reguljära uttryck och hur de fungerar kan hjälpa dig att bli mer effektiv i ditt arbete med att ta bort tecken som matchar ett visst mönster i en textsträng.

## Se även

- [Ruby's string manipulation methods](https://ruby-doc.org/core-2.5.1/String.html#method-i-gsub-21)
- [Ruby's regular expressions documentation](https://ruby-doc.org/core-2.5.1/Regexp.html)