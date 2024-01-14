---
title:                "Javascript: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
I en mångfald av program som vi använder i vardagen, vare sig det är ett spel, en app eller en finansapplikation, används slumpmässiga tal i olika funktioner. Att kunna generera slumpmässiga tal är en viktig del av Javascript-programmering och hjälper till att skapa mer dynamiska och interaktiva användarupplevelser.

## Hur man gör det
För att generera slumpmässiga tal i Javascript kan man använda sig av inbyggda funktioner och metoder. Ett sätt är att använda Math.random() som returnerar ett tal mellan 0 och 1. Om man vill ha ett heltal kan man använda Math.floor(Math.random() * n) där n är det övre gränsvärdet för intervallet av de slumpmässiga talen. Här är ett exempel på hur man kan använda Math.random() för att generera tre slumpmässiga tal mellan 1 och 10:

```Javascript
let num1 = Math.floor(Math.random() * 10) + 1;
let num2 = Math.floor(Math.random() * 10) + 1;
let num3 = Math.floor(Math.random() * 10) + 1;
console.log(num1, num2, num3); /* Exempel på utskrift: 6 2 9 */
```

Man kan också använda en loop för att generera flera slumpmässiga tal. Till exempel kan man använda en while-loop för att generera 10 slumpmässiga tal mellan 1 och 20 och sedan lägga till dem i en array:

```Javascript
let numbers = [];
let i = 0;
while (i < 10) {
    let num = Math.floor(Math.random() * 20) + 1;
    numbers.push(num);
    i++;
}
console.log(numbers); /* Exempel på utskrift: [17, 5, 11, 9, 3, 19, 13, 8, 20, 1] */
```

## Djupdykning
När man använder Math.random() ska man vara medveten om att det inte är en riktig slumpgenerator utan en så kallad pseudo slumpgenerator. Det innebär att talen som genereras inte är helt slumpmässiga utan följer en matematisk formel baserad på ett startvärde, också kallat seed. Därför är det viktigt att se till att man inte använder samma seed varje gång man kör programmet, annars kommer samma tal att genereras varje gång.

För att undvika detta kan man använda en seed baserad på tiden, som t.ex. Date.now(), vilket gör att varje gång programmet körs blir seed:et unikt och därmed genereras olika slumpmässiga tal. En annan möjlighet är att använda en extern slumpgenerator från ett extern bibliotek eller API.

## Se även
- [Math.random() - MDN webbdokumentation](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Globala_objekt/Math/random)
- [Pseudo slumpgenerator - Wikipedia](https://sv.wikipedia.org/wiki/Pseudoslumptal)
- [Tidsbaserad seed i Javascript - StackOverflow](https://stackoverflow.com/questions/1349404/generate-random-string-characters-in-javascript)