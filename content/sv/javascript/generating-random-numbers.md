---
title:    "Javascript: Generering av slumpmässiga nummer"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför
Att skapa slumpmässiga nummer är en viktig del av många programmeringsprojekt. Oavsett om du behöver ett slumpmässigt lösenord, skapar ett spel eller testar algoritmer, kan generering av slumpmässiga nummer vara en användbar funktion i ditt program.

## Hur man gör
För att generera slumpmässiga nummer i Javascript använder man sig av funktionen Math.random(). Detta ger ett flyktigt värde mellan 0 och 1. För att få ett heltal kan man sedan använda sig av Math.floor() eller Math.ceil() för att avrunda nedåt respektive uppåt till närmaste heltal.

```Javascript
// Generera ett slumpmässigt tal mellan 1 och 10
let randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum); // Output: Slumpmässigt tal mellan 1 och 10
```

Om du vill ha ett slumpmässigt tal mellan två specifika värden, till exempel mellan 20 och 50, kan du använda samma metod men lägga till det nedre värdet som en konstant i uträkningen.

```Javascript
// Generera ett slumpmässigt tal mellan 20 och 50
let randomNum = 20 + Math.floor(Math.random() * 31);
console.log(randomNum); // Output: Slumpmässigt tal mellan 20 och 50
```

Om du vill ha en lista med slumpmässiga värden kan du använda en loop för att generera flera tal.

```Javascript
// Generera en lista med 10 slumpmässiga tal mellan 1 och 100
let randomList = [];
for (let i = 0; i < 10; i++) {
  randomList.push(Math.floor(Math.random() * 100) + 1);
}
console.log(randomList); // Output: Lista med 10 slumpmässiga tal mellan 1 och 100
```

## Djupdykning
Det finns flera faktorer som kan påverka hur slumpmässiga de genererade talen faktiskt är. Eftersom Math.random() använder sig av ett flyktigt värde, kan det påverkas av timestampen som används av datorn för att beräkna det. Detta kan i vissa fall leda till att uträkningen blir mindre slumpmässig. Det finns dock olika tekniker som kan användas för att förbättra slumpmässigheten, såsom att blanda olika algoritmer eller använda andra värden, som användarens muspekare, vid uträkningsprocessen.

## Se även
- [Math Objektet i Javascript](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Math)
- [Användarnas muspekare för att förbättra slumpmässighet](https://medium.com/@madsborup/random-with-mouse-position-edfd309ca5a4)
- [Slumpmässighet i datorprogram](https://en.wikipedia.org/wiki/Random_number_generation)