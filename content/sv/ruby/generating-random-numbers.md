---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Generering av slumpmässiga nummer är processen för att skapa ett nummer på ett till synes oberäkneligt sätt. Programmerare gör detta för att skapa unika identiteter, simulera händelser, upprätta datasäkerhet, och mer.

## Hur man:

Att skapa random nummer i Ruby är ganska enkelt. Vi kommer att använda `rand` metoden.

```Ruby
#Genererar ett random nummer mellan 0 och 1
puts rand 
```
Detta kommer att generera ett nummer mellan 0 och 1. Om du vill generera ett nummer inom ett särskilt intervall kan du gör det så här:

```Ruby
#Genererar ett random nummer mellan 1 och 10
puts rand(1..10) 
```

## Fördjupning

Historiskt har genereringen av random nummer vanligtvis varit en svår uppgift. Metoderna inkluderar allt från att slå tärningar och lotteridragningar till avancerade statistiska metoder. I modern programmering kan vi dock generera slumpmässiga nummer med en rad olika verktyg och tekniker.

Som ett alternativ till Ruby's inbyggda `rand`, kan du också kolla in SecureRandom modulen, som genererar slumpmässiga nummer av en högre kryptografisk kvalitet. 

När det gäller implementation, fungerar Ruby's `rand` metod så att den faktiskt använder systemets källa till slumpmässighet (till exempel /dev/urandom på Unixliknande system), som matas in i en algoritm för att generera det slumpmässiga numret.

## Se även

För mer information om att generera random nummer, se följande källor:

- Ruby Dokumentation om [`rand`](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-rand) och [`SecureRandom`](https://ruby-doc.org/stdlib-2.5.1/libdoc/securerandom/rdoc/SecureRandom.html)
- Artikel om [Random Number Generation](https://en.wikipedia.org/wiki/Random_number_generation) på Wikipedia.
- Inlägg om [How To Generate Random Numbers In Ruby](https://www.rubyguides.com/2015/03/ruby-random/) på RubyGuides.