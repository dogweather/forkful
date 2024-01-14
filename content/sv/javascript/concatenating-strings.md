---
title:    "Javascript: Sammanslående strängar"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför
Att använda sig av strängkonkatinering är ett viktigt koncept inom Javascript-programmering eftersom det möjliggör att kombinera flera strängar till en enda och på så sätt skapa dynamiska och anpassningsbara texter som kan användas för olika ändamål.

## Hur man gör
För att konkatenera strängar i Javascript behöver du använda dig av operatorn "+" som fungerar som en slags sammanfogare. Nedan finns ett enkelt exempel på hur man kan använda sig av denna operator för att kombinera två strängar till en enda:

```Javascript
var fornamn = "Anna";
var efternamn = "Larsson";

var namn = fornamn + " " + efternamn;
console.log(namn);

// Output: "Anna Larsson"
```

I exemplet ovan används "+"-operatorn för att skapa en ny variabel som innehåller både förnamnet och efternamnet, separerade med ett mellanslag. Genom att sedan logga ut den nya variabeln får vi som resultat den konkatenerade strängen "Anna Larsson".

## Fördjupning
Det finns flera sätt att konkatenera strängar på, beroende på vilka behov och preferenser man har. En vanlig metod är användningen av metoden "concat()", som är speciellt utformad för strängkonkatinering och som kan användas för att konkatenera flera strängar samtidigt.

```Javascript
var text1 = "Hej ";
var text2 = "där!";

var text3 = text1.concat(text2);
console.log(text3);

// Output: "Hej där!"
```

Det är även möjligt att konkatenera strängar med hjälp av template literals, vilket gör det enklare att integrera variabler i de strängar man vill konkatenera.

```Javascript
var stad = "Stockholm";
var text4 = `Jag bor i ${stad}.`;
console.log(text4);

// Output: "Jag bor i Stockholm."
```

## Se även
- [MDN Web Docs - Strängkonkatinering](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition_assignment)
- [W3Schools - Javascript Strängar](https://www.w3schools.com/js/js_strings.asp)