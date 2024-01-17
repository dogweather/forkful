---
title:                "Generering av slumpmässiga tal"
html_title:           "Ruby: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

Vad & Varför?
Generering av slumpmässiga nummer är en vanlig uppgift för programmerare. Det innebär att skapa en serie med nummer som är slumpmässigt valda, utan något specifikt mönster. En anledning till att programmerare gör detta är för att skapa variation i sitt program, till exempel för att simulera ett slumpmässigt beteende eller för att utföra tester.

Hur man:
```ruby
# Genererar ett slumpmässigt nummer mellan 1 och 10
rand(1..10) 
# Output: 7

# Genererar ett slumpmässigt flyttal mellan 0.0 och 1.0
rand() 
# Output: 0.7356200041991567

# Genererar ett slumpmässigt heltal mellan 1 och 100
Random.rand(1..100) 
# Output: 42

# Väljer ett slumpmässigt element från en array
array = ["a", "b", "c", "d"]
array.sample 
# Output: "c"
```

Utforska:
Att generera slumpmässiga nummer är en viktig del av datavetenskap och har funnits sedan de tidiga dagarna av datorer. Tidigare användes metoderna pseudo-random för att simulera slumpmässighet, men idag finns det bättre algoritmer som ger mer verkligt slumpmässiga nummer. Alternativ till att använda Ruby's inbyggda funktioner är att skapa sin egen algoritm för slumpmässighet, eller använda en tredjepartsbibliotek.

Se även:
- Ruby's inbyggda dokumentation om Random-klassen: https://ruby-doc.org/core-2.7.1/Random.html
- En förklaring av olika algoritmer för att generera slumpmässiga nummer: https://www.random.org/randomness/