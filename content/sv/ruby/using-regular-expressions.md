---
title:                "Ruby: Att använda reguljära uttryck"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regular expressions eller reguljära uttryck är ett kraftfullt verktyg inom programmering som används för att matcha och manipulera text. Genom att lära sig hur man använder reguljära uttryck kan du spara tid och skriva effektivare kod.

## Så här gör du

För att använda reguljära uttryck i Ruby måste du först importera RegExp biblioteket med `require 'regexp'`. Sedan kan du använda metoden `.match` för att hitta matchningar i en sträng. Till exempel:

```Ruby
str = "Hej, jag heter Ruby och jag är en programmeringsspråk."
match = /Ruby/.match(str)
puts match[0]
```

Detta kommer att skriva ut "Ruby" eftersom det är det enda ordet som matchar i vår sträng.

Du kan också använda reguljära uttryck för att ersätta text med `.gsub` metoden. Till exempel:

```Ruby
str = "Jag gillar att läsa och skriva kod."
puts str.gsub(/skriva/, "programmera")
```

Detta kommer att skriva ut "Jag gillar att läsa och programmera kod." eftersom vi ersatte ordet "skriva" med "programmera".

## Djupdykning

Reguljära uttryck kan verka komplicerade till en början, men när du väl förstår grunderna kan de vara mycket kraftfulla. Du kan matcha mönster av tecken, siffror eller ens hitta specifika ord och uttryck med hjälp av olika metoder och symboler inom reguljära uttryck.

Här är några användbara symboler som du kan använda i dina reguljära uttryck:

* `.` - matchar ett tecken
* `*` - matchar 0 eller flera förekomster
* `+` - matchar 1 eller flera förekomster
* `?` - matchar 0 eller 1 förekomst
* `\d` - matchar en siffra
* `\w` - matchar en bokstav eller siffra

Det finns också många inbyggda metoder som du kan använda på dina reguljära uttryck för att göra dem ännu mer kraftfulla. Se länkarna nedan för mer djupgående information och exempel.

## Se också

* [Ruby Regex Cheat Sheet](https://www.rubyinside.com/media/images/ruby-regular-expressions-cheat-sheet.pdf)
* [Ruby's Regexp dokumentation](https://ruby-doc.org/core-2.7.1/Regexp.html)
* [Regexr](https://regexr.com/) - ett verktyg för att experimentera och testa reguljära uttryck online.