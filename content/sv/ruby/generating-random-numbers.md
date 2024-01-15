---
title:                "Generering av slumpmässiga nummer"
html_title:           "Ruby: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är en vanlig uppgift inom programmering, särskilt inom spelutveckling och statistiska beräkningar. Det kan också användas för att skapa variation och slumpmässighet i olika applikationer och algoritmer.

## Hur man gör det
För att generera slumpmässiga nummer i Ruby kan vi använda oss av inbyggda metoder i språket. Ett sätt är att använda sig av `rand` metoden som returnerar ett decimaltal mellan 0 och 1. För att få ett heltal kan vi multiplicera resultatet med antalet önskade tal och sedan avrunda det till närmaste heltal.

Exempel:

```Ruby
# Genererar ett slumpmässigt heltal mellan 1 och 10
random_number = (rand * 10).round 

puts random_number # Exempeloutput: 7
```

Om vi vill ha ett slumpmässigt nummer inom ett visst intervall, till exempel mellan 50 och 100, kan vi använda oss av `rand(min..max)` metoden. Den tar ett internt nummer som argument och returnerar ett tal inom det givna intervallet.

Exempel:

```Ruby
# Genererar ett slumpmässigt nummer mellan 50 och 100
random_number = rand(50..100)

puts random_number # Exempeloutput: 82
```

## Djupdykning
För att förstå hur `rand` metoden fungerar kan vi titta närmare på dess implementation. I Ruby är `rand` en del av `Random` klassen, som vi kan använda för att skapa en instans och sedan göra anrop till dess metoder, som `rand`.

Därför kan vi, om vi vill ha mer kontroll över hur slumpmässiga nummer genereras, skapa en egen instans av `Random` och ange ett seed-värde för att få ett bestämt mönster i nummergenereringen.

Exempel:

```Ruby
# Skapar en instans av Random och anger seed-värde
random_generator = Random.new(123)

# Genererar 5 slumpmässiga heltal mellan 1 och 10
5.times { puts random_generator.rand(10) }
# Exempeloutput: 8, 3, 6, 2, 4

# Genererar samma 5 heltal eftersom vi angett ett seed-värde
5.times { puts random_generator.rand(10) }
# Exempeloutput: 8, 3, 6, 2, 4
```

Slutligen är det viktigt att vara medveten om att de slumpmässiga nummer som genereras i Ruby inte är helt och hållet slumpmässiga utan baseras på en algoritm. För mer avancerade behov av slumpmässighet, till exempel inom kryptografi, finns det andra metoder som kan användas.

## Se också
Om du vill lära dig mer om slumpmässiga nummer i Ruby kan du titta på följande länkar:

- [Dokumentation för `Random` klassen](https://ruby-doc.org/core/Random.html)
- [Mer om slumpmässighet i Ruby](https://www.rubyguides.com/2018/11/ruby-random/)
- [Använda `SecureRandom` för säkrare slumpmässiga nummer](https://www.youtube.com/watch?v=7txDRDV7er0) (Video)