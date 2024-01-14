---
title:                "Ruby: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

Rubi-programmering: Enkel guide för att använda reguljära uttryck

## Varför

Reguljära uttryck, även kända som regex, är ett kraftfullt verktyg för textmönstermatchning i Ruby-programmering. Genom att använda regex kan du enkelt hitta och manipulera specifika delar av en sträng. Detta kan vara användbart för att validera inmatade data eller för att söka och ersätta text i en större sträng. Det sparar tid och gör kodning mer effektivt.

## Hur man använder

För att använda reguljära uttryck i Ruby behöver du bara lägga till "require 're'" i ditt program. Därefter kan du skapa ett reguljärt uttryck som ett objekt och använda metoder som match, scan och sub för att hitta och manipulera text. Här är ett exempel på hur man använder en reguljär uttryck för att hitta en e-postadress i en sträng:

```Ruby
str = "Kontakta mig på myemail@domain.com för mer information."
regex = /[a-z]+@[a-z]+\.[a-z]+/
match = regex.match(str)
puts match[0] # myemail@domain.com
```

I detta exempel används reguljärt uttryck för att hitta en e-postadress som följer det typiska formatet "användarnamn@domän.com". Notera att "match[0]" returnerar hela matchningen, medan "match[1], match[2], osv." skulle returnera varje grupp i uttrycket separat.

## Djupdykning

Reguljära uttryck kan vara förvirrande för nybörjare, men att förstå grunderna kommer att göra det mycket enklare. Här är några viktiga saker att komma ihåg när du använder reguljära uttryck:

- Teckenklasser ser till att dina uttryck endast matchar specifika tecken, till exempel [a-z] för att matcha alla små bokstäver eller [0-9] för att matcha alla siffror.
- Kvantisering används för att ange hur många gånger en viss del av uttrycket ska matcha, till exempel \d{3} för att matcha exakt tre siffror eller \w+ för att matcha en eller flera ord-tecken.
- Gruppering och fångst tillåter dig att isolera delar av en matchning för att sedan använda den i din kod.

För mer information om reguljära uttryck kan du konsultera Ruby-dokumentationen eller använda onlineverktyg som regex101.com för att testa och experimentera med dina uttryck.

## Se även

- Ruby-dokumentation om reguljära uttryck: https://ruby-doc.org/core-2.5.3/Regexp.html
- Tutorial för reguljära uttryck i Ruby: https://www.rubyguides.com/2015/06/ruby-regex/
- Regex101 för att testa reguljära uttryck online: https://regex101.com/