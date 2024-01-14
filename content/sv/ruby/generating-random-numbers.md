---
title:    "Ruby: Skapande av slumpmässiga nummer"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är ett viktigt koncept inom programmering, särskilt inom Ruby. Det är ett verktyg som kan användas för att skapa variation i din kod och öka dess flexibilitet. Det är också ett användbart koncept för spel- eller simuleringsapplikationer där slumpmässighet behövs.

## Hur man gör

Ruby har inbyggda metoder för att generera slumpmässiga nummer, vilket gör det till en enkel process. En av dessa metoder är .rand, som returnerar ett värde mellan 0 och 1. För att få ett heltal mellan ett visst intervall kan du multiplicera .rand med önskat högsta tal och sedan lägga till det lägsta talet. Till exempel:

```Ruby
rand * 10 + 1  # Ger ett tal mellan 1 och 10
```

Du kan också använda en annan metod, .rand(range), som tar ett intervall som argument och returnerar ett slumpmässigt tal inom det intervallet. Till exempel:

```Ruby
rand(1..10) # Ger ett tal mellan 1 och 10
```

Om du behöver en slumpmässig bokstav kan du använda .chr, som kommer att returnera en slumpmässig bokstav från alfabetet. Till exempel:

```Ruby
(65 + rand(26)).chr  # Ger en slumpmässig stor bokstav
```

Du kan också använda .shuffle för att blanda om en array, vilket kan vara användbart för att skapa en slumpmässig ordningsföljd av element. Till exempel:

```Ruby
["a", "b", "c", "d"].shuffle # Ger ett slumpmässigt ordnat array
```

## Djupdykning

När det gäller att generera slumpmässiga nummer är det viktigt att förstå att dessa nummer egentligen inte är helt slumpmässiga. De skapas med hjälp av en algoritm som försöker att efterlikna det beteende som vi skulle associera med slumpmässighet. Det finns olika algoritmer för detta, och det är viktigt att välja en som passar för ditt specifika syfte.

En annan sak att tänka på är att slumpmässiga nummer är resultatet av en startpunkt, så om du kör samma kod flera gånger kommer du att få samma sekvens av nummer. Om du vill undvika detta kan du ge ett "seed"-värde till .rand-metoden och därmed få en annan sekvens av slumpmässiga nummer varje gång.

## Se även

- [Ruby Dokumentation: Random](https://ruby-doc.org/core-2.7.1/Random.html)
- [Ruby Dokumentation: Kernel#rand](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-rand)
- [Hvad mere kan du gøre med Ruby's Random clas](https://blog.appacademy.io/what-else-can-you-do-with-rubys-random-class/)