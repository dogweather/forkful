---
title:                "Ruby: Skapa slumpmässiga nummer"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är ett vanligt behov inom många programmeringsprojekt. Det kan användas för att skapa unika identifierare, simuleringsprogram eller helt enkelt för att lägga till slumpmässiga element i ditt program.

## Hur man gör
För att skapa slumpmässiga nummer i Ruby kan du använda `rand` funktionen. Här är ett exempel på hur du genererar ett slumpmässigt heltal mellan 1 och 10:

```Ruby
rand(1..10)
```

Detta kommer att ge dig ett slumpmässigt tal varje gång du kör programmet. Om du istället vill ha ett slumpmässigt decimaltal kan du använda följande kod:

```Ruby
rand(0.0..10.0)
```

### Flera sätt att generera slumpmässiga nummer
Det finns flera andra sätt att generera slumpmässiga nummer i Ruby. Ett annat alternativ är att använda `Random` klassen och dess `rand` funktion:

```Ruby
Random.rand(1..10)
```

Du kan också använda `Kernel#srand` funktionen för att seeda slumpen, vilket gör att du får samma resultat varje gång du kör programmet med samma seed. Här är ett exempel:

```Ruby
srand(12345)
rand(1..10) # kommer alltid att ge samma resultat
```

## Djupdykning
Bakom kulisserna använder Ruby Mersenne Twister algoritmen för att generera slumpmässiga nummer. Detta är en välkänd och pålitlig metod för att skapa slumpmässiga nummer. Det är också möjligt att ange en seed för `rand` eller `Random.rand` funktionen för att få olika sekvenser av slumpmässiga nummer.

En intressant egenskap hos Mersenne Twister är att den kan generera otroligt många unika nummer innan den börjar upprepa sig. Detta gör den till en bra kandidat för spel och andra system där slumpen är viktig.

## Se även
* [Dokumentation för `rand` funktionen](https://ruby-doc.org/core-3.0.2/Random.html#method-i-rand)
* [Mersenne Twister algoritmen](https://en.wikipedia.org/wiki/Mersenne_Twister)
* [Exempel på användning av `rand` funktionen i Ruby](https://www.rubyguides.com/2017/07/ruby-random/)