---
title:                "Gleam: Skapa slumpmässiga tal"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är en viktig del av många program. Det kan användas för att skapa olika användare, utföra slumpvisa tester eller spela ett slumpmässigt spel. I denna bloggpost kommer vi att diskutera hur man kan använda Gleam för att enkelt generera slumpmässiga nummer i dina program.

## Hur man gör
För att generera slumpmässiga nummer i Gleam kan du använda den inbyggda funktionen `random.uniform()` som tar två tal som parametrar och returnerar ett slumpmässigt nummer mellan dem. Låt oss ta en titt på ett exempel:

```Gleam
let random_number = random.uniform(1, 10)
```

I detta exempel kommer `random_number` att tilldelas ett slumpmässigt nummer mellan 1 och 10. Du kan också använda `random.pick()` för att välja ett slumpmässigt element från en lista:

```Gleam
let fruits = ["apple", "banana", "orange", "kiwi"]
let random_fruit = random.pick(fruits)
```

I detta exempel kommer `random_fruit` att välja ett slumpmässigt element från listan `fruits` och tilldela det till variabeln.

## Djupdykning
För mer komplexa användningsområden kan du också använda `random.seed()` för att generera en given sekvens av slumpmässiga nummer baserade på ett visst startvärde. Detta kan vara användbart för exempelvis simuleringar eller spel där det är viktigt att samma sekvens av slumpmässiga nummer används varje gång programmet körs. Du kan också använda `random.shuffle()` för att blanda en lista på ett slumpmässigt sätt.

## Se även
- Officiell Gleam-dokumentation: [Random Modul](https://gleam.run/modules/random/)
- Gleam Cookbook: [Generating Random Numbers](https://www.gleam.run/cookbook/random-number-generation/)
- Gleam Discord-community: [Slumpmässiga nummer-kanalen](https://discord.gg/Qk7a9W6d)