---
title:                "Fish Shell: Slumpmässig generering av nummer"
simple_title:         "Slumpmässig generering av nummer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att skapa slumpmässiga tal är ofta en viktig del av programutveckling och kan ha många olika användningsområden, från spel till statistiska beräkningar. Genom att använda Fish Shell kan du enkelt generera slumpmässiga nummer och integrera dem i dina program.

## Hur man gör

Att skapa slumpmässiga tal i Fish Shell är enkelt med hjälp av det inbyggda kommandot `random`. För att generera ett slumpmässigt heltal mellan 1 och 100 kan du använda följande kod:

```Fish Shell
random 1 100
```

Om du istället vill ha ett slumpmässigt decimaltal mellan 1 och 10 kan du använda följande kod:

```Fish Shell
random --float 1 10
```

För att generera flera slumpmässiga tal i en loop kan du använda kommandot `seq` tillsammans med `random`, som i följande kod:

```Fish Shell
for i in (seq 1 10)
    echo (random 1 100)
end
```

Det finns även flera olika flaggor som du kan använda tillsammans med kommandot `random` för att styra hur indata och utdata ska hanteras. Du kan lära dig mer om dessa flaggor genom att köra kommandot `man random` i Fish Shell.

## Djupdykning

Bakom kulisserna använder Fish Shell en pseudo-slumpgenerator baserad på mönstret "Xorshift32". Det är en av de snabbaste slumpgeneratorerna tillgängliga och ger en god balans mellan kvalitet och hastighet. Det betyder att de genererade talen är tillräckligt slumpmässiga för de flesta ändamål, men om du behöver en hög nivå av säkerhet bör du använda en annan slumpgenerator.

För att generera en mer komplex och skräddarsydd sekvens av slumpmässiga tal kan du använda `mt19937`-algoritmen. Den är mer avancerad och erbjuder en betydligt högre nivå av slumpmässighet, men den är också långsammare än Xorshift32. Du kan läsa mer om denna algoritm genom att köra kommandot `man mt19937` i Fish Shell.

## Se även

Eftersom slumpgeneratorer är en viktig del av många programmeringsspråk finns det gott om resurser där du kan lära dig mer om dem och deras olika implementeringar. Här är några användbara länkar:

- [Random Numbers in Programming Languages](https://www.geeksforgeeks.org/random-numbers-in-programming-languages/)
- [Random Number Generation in Python](https://realpython.com/python-random/)
- [The Importance of Randomness in Programming](https://medium.com/swlh/the-importance-of-randomness-in-programming-a331cac12fcc)