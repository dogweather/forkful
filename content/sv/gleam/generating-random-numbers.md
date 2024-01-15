---
title:                "Skapa slumpmässiga tal"
html_title:           "Gleam: Skapa slumpmässiga tal"
simple_title:         "Skapa slumpmässiga tal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Varför
Vi genererar slumpmässiga tal av många olika skäl - från spel och lotterier till simuleringar och kryptografiska tillämpningar. Oavsett anledning är det viktigt att kunna skapa slumpmässiga tal som är riktigt slumpmässiga och inte kan förutsägas.

##Hur man gör
För att generera slumpmässiga tal i Gleam använder vi modulen "re" som innehåller en mängd olika funktioner för att hantera slumpmässiga tal. Först måste vi importera modulen med hjälp av `import re`.

För att få ett slumpmässigt tal mellan ett minimumvärde och ett maximumvärde kan vi använda `random.int/1` funktionen:

```Gleam
let slumpmässigt_tal = 
    // Minsta värde = 1
    // Största värde = 10
    re.random.int(1, 10)
// Output: slumpmässigt_tal = 7
```

Om vi vill ha ett slumpmässigt flyttal mellan 0 och 1 kan vi använda `random.float/0` funktionen:

```Gleam
let flyttal = re.random.float()
// Output: flyttal = 0.57493230
```

För att få ett slumpmässigt tal från en lista eller en tuple kan vi använda `random.pick/1` funktionen:

```Gleam
let tal_lista = [1, 2, 3, 4, 5]
let slumpmässigt_tal = re.random.pick(tal_lista)
// Output: slumpmässigt_tal = 3 (kan vara ett annat tal vid varje körning)
```

Det finns också möjlighet att generera slumpmässiga strängar med hjälp av `random.string/1` funktionen:

```Gleam
let slumpmässig_sträng = re.random.string(8) // 8 är längden på strängen
// Output: slumpmässig_sträng = "Rp!k9&cD" (kan vara en annan sträng vid varje körning)
```

##Djupdykning
Du kanske undrar hur dessa slumpmässiga tal faktiskt genereras. Det finns flera olika metoder som kan användas för att generera slumpmässiga tal, men den metod som Gleam använder sig av är känd som Pseudo Random Number Generator (PRNG).

PRNG använder en algoritm för att beräkna en följd av nummer som framstår som slumpmässig. Algoritmen använder ett initialt värde, som kallas en seed, för att skapa denna följd. Om seeden är densamma kommer följderna av slumpmässiga tal att vara identiska.

I Gleam används en algoritm som kallas "Mersenne Twister" för att generera slumpmässiga tal. Den är känt för att ge en hög grad av slumpmässighet och är också snabb och effektiv.

Nu när du har en grundläggande förståelse för hur man genererar slumpmässiga tal i Gleam, kan du experimentera med de olika funktionerna för att skapa numeriska och icke-numeriska slumpmässiga värden.

##Se även
- Gleam dokumentation: https://gleam.run/documentation/
- PRNG på Wikipedia (på svenska): https://sv.wikipedia.org/wiki/Pseudo-random_generator
- Mersenne Twister på Wikipedia (på svenska): https://sv.wikipedia.org/wiki/Mersenne_Twister