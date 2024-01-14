---
title:                "Python: Generering av slumpmässiga tal"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

Varför generate random numbers

Att generera slumpmässiga nummer är ett vanligt koncept inom programmering, särskilt inom dataanalys och simuleringar. Det kan hjälpa till att skapa orealistiska data och scenarier för att testa programmets hållbarhet och förutsäga olika möjliga resultat.

Hur man gör det

```Python
import random
#Genererar ett slumpmässigt nummer mellan 1 och 10
random_nummer = random.randint(1, 10)
print(random_nummer)
#Output: 7
```

I koden ovan har vi använt funktionen "randint" från modulen "random" för att generera en slumpmässig integer mellan 1 och 10. Detta är bara ett enkelt exempel på hur man kan använda random-nummer i sitt program, men det finns många fler sätt att göra det på.

En annan vanlig funktion för att generera slumpmässiga nummer är "random.random", som genererar ett decimaltal mellan 0 och 1. Detta kan vara användbart vid simuleringar där man behöver en variabel som är kontinuerlig istället för diskret. Se nedan för ett exempel:

```Python
import random
#Genererar ett decimaltal mellan 0 och 1
random_nummer = random.random()
print(random_nummer)
#Output: 0.543217
```

Man kan också använda sig av listor för att ange ett visst utbud av nummer som man vill generera slumpmässigt från. Se nedan för exempel:

```Python
import random
#Skapar en lista med fem olika tal
tal_lista = [1, 4, 7, 11, 23]
#Genererar ett slumpmässigt tal från listan ovan
random_nummer = random.choice(tal_lista)
print(random_nummer)
#Output: 7
```

Djupdykning

Att generera slumpmässiga nummer är inte så slumpmässigt som man kanske tror. Inom programmering används ofta något som kallas en "pseudo-random number generator" (PRNG) för att skapa dessa nummer. Det är en algoritm som genererar sekvenser av nummer som kan uppfattas som slumpmässiga, men i själva verket är de baserade på en startpunkt eller "seed". Om samma seed används kommer sekvensen av nummer alltid att vara densamma.

Detta kan vara användbart vid testning av program, men om man vill ha riktigt slumpmässiga nummer måste man använda sig av yttre faktorer, som processors hastighet eller användarens muspekare, för att skapa seed-värden.

Se även

- Dokumentation för random-modulen i Python: https://docs.python.org/3/library/random.html
- En förklaring av hur PRNG fungerar: https://en.wikipedia.org/wiki/Pseudorandom_number_generator