---
title:                "Användning av reguljära uttryck"
html_title:           "Ruby: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck, ofta förkortade som regex, är ett praktiskt verktyg som hjälper programmerare att söka och manipulera text data på ett effektivt sätt genom användande av mönster. Vanligtvis används de för att söka efter och extrahera specifika delar av en text eller för att ersätta ett visst mönster med annan information. Regex kan också användas för att validera indata och hantera felmeddelanden. Många programmeringsspråk, inklusive Ruby, har inbyggda stöd för reguljära uttryck.

## Hur man:

För att använda reguljära uttryck i Ruby, används två främsta metoder: `match` och `gsub`. `Match` används för att hitta en matchning av ett visst mönster i en textsträng och returnerar det matchande resultatet. Till exempel, om vi vill hitta alla siffror i en text kan vi använda följande kod:
```Ruby
text = "Det finns 10 olika typer av människor i världen"
match = text.match(/\d+/)
puts match #kommer att skriva ut "10" 
```
`Gsub` däremot, används för att ersätta olika delar av en text enligt ett givet mönster. Till exempel, om vi vill byta ut alla förekomster av bokstaven "e" med bokstaven "a" i en text kan vi använda följande kod:
```Ruby
text = "Hello World!"
new_text = text.gsub(/e/, "a")
puts new_text #kommer att skriva ut "Hallo Warld!" 
```

## Deep Dive:

Reguljära uttryck skapades ursprungligen av matematikern Stephen Cole Kleene under 1950-talet. Då användes de i språkteorin för att beskriva formella språk. Det var först under 1960-talet som Ken Thompson och Dennis Ritchie implementerade reguljära uttryck i programvaran för operativsystemet Unix.

Alternativ till reguljära uttryck inkluderar att använda inbyggda metoder som `include?` eller `scan` för att söka efter en viss text i en sträng. En annan möjlighet är att använda olika XML-parser för att söka och manipulera data.

Implementeringen av reguljära uttryck i Ruby bygger på ett bibliotek som heter Oniguruma, som är skrivet i C. Detta bibliotek ger snabb prestanda och stöd för Unicode-teckenkodning. Ruby har en lättanvänd syntax för reguljära uttryck, vilket gör det enkelt för utvecklare att lära sig och implementera dem i sina projekt.

## Se även:

Om du vill lära dig mer om reguljära uttryck i Ruby, rekommenderar jag att läsa dokumentationen på [Ruby-doc.org](https://ruby-doc.org/core-3.0.0/Regexp.html). Du kan också titta på [The Ruby Regexp Bible](https://www.rubyguides.com/2015/06/ruby-regex/) för djupare förståelse och användbara exempel. Andra relevanta källor inkluderar [Ruby Regular Expression Tutorial](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm) och [Regular Expressions i Ruby](https://www.rubyguides.com/2015/06/ruby-regex/).

Nu när du har en grundläggande förståelse för reguljära uttryck och hur man använder dem i Ruby, är det dags att experimentera och använda dem i dina egna projekt!