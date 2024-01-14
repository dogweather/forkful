---
title:                "Ruby: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är ett viktigt koncept inom programmering. Det kan användas för spel, simuleringsprogram, säkerhet och mycket mer. Att lära sig hur man skapar slumpmässiga nummer i Ruby är en viktig färdighet för alla som vill bli en bättre programmerare.

## Hur man gör

För att generera slumpmässiga nummer i Ruby kan du använda funktionen `rand()`. Detta kommer att skapa ett decimaltal mellan 0 och 1. Om du vill skapa slumpmässiga heltal kan du använda funktionen `rand()` tillsammans med `*` för att multiplicera med ett nummer, till exempel `rand() * 10` för att få ett heltal mellan 0 och 10.

```Ruby
# Exempel på att generera slumpmässiga heltal
# mellan 1 och 10
puts rand() * 10

# Exempel på att generera slumpmässiga decimaltal
# mellan 0 och 1
puts rand()
```

Output:
```Ruby
7
0.6421782193
```

Du kan också använda `rand(x)` där x är ett heltal för att få ett slumpmässigt heltal mellan 0 och x. Om du vill ha ett positivt heltal mellan x och y kan du använda `rand(x..y)`.

För att kontrollera vilka nummer som kommer att genereras kan du använda `srand(nummer)`, där nummer är en specifierad startpunkt för den slumpmässiga sekvensen. Detta är användbart om du till exempel vill återskapa samma slumpmässiga tal i ett program i framtiden.

## Djupdykning

Bakom kulisserna använder Ruby en algoritm som kallas Mersenne Twister för att generera slumpmässiga nummer. Detta är en av de mest utbredda algoritmerna för att skapa slumpmässiga sekvenser och är baserad på en matematisk formel. Användningen av `srand()` ändrar startvärdet för denna formel, vilket resulterar i olika sekvenser av slumpmässiga nummer.

Det finns också andra sätt att generera slumpmässiga tal i Ruby, inklusive att använda tillägget `SecureRandom`. Detta ger ett extra lager av säkerhet genom att använda en kryptografiskt säker algoritm för att skapa slumpmässiga nummer.

## Se även

- [Slumpmässiga nummer i Ruby dokumentationen](https://ruby-doc.org/core-3.0.1/Random.html)
- [Mersenne Twister på Wikipedia](https://sv.wikipedia.org/wiki/Mersenne_Twister)
- [SecureRandom dokumentationen](https://ruby-doc.org/stdlib-3.0.1/libdoc/securerandom/rdoc/SecureRandom.html)