---
title:    "Javascript: Sammanslagning av strängar"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför:
Det finns många användbara metoder inom programmering, men att kunna sammanfoga strängar är en av de mest grundläggande och viktiga. Genom att kunna kombinera flera strängar till en enda kan du skapa dynamiska och anpassningsbara texter som bidrar till en bättre användarupplevelse i dina program och applikationer.

## Hur du gör:
Det finns flera sätt att sammanfoga strängar i Javascript, men den enklaste metoden är att använda operatorn "+" för att lägga ihop dem. Se nedan för kodexempel och resultat:

```Javascript
let förnamn = "Johan";
let efternamn = "Svensson";
let helsnamn = förnamn + " " + efternamn;

console.log(helsnamn); 

//Output: Johan Svensson
```

Som du kan se ovan använde vi "+" för att lägga till en mellanslag mellan förnamn och efternamn. Du kan också använda denna metod för att sammanfoga flera strängar på en gång:

```Javascript
let citat = "Programmering är " + "som att lösa ett pussel " + "med kod.";

console.log(citat); 

//Output: Programmering är som att lösa ett pussel med kod.
```

En annan metod för att sammanfoga strängar är genom att använda "Template Literals" som är en modernare och mer läsbar metod. Se exempel nedan:

```Javascript
let favoritMat = "pasta";
let beskrivning = `Min favoritmat är ${favoritMat}.`;

console.log(beskrivning); 

//Output: Min favoritmat är pasta.
```

Som du kan se använde vi "Template Literals" för att lägga till en variabel i en sträng utan att behöva använda "+"-operatorn.

## Djupdykning:
När man sammanfogar strängar i Javascript är det viktigt att komma ihåg att det resulterande värdet alltid blir en ny sträng, oavsett vilken metod du använder. Detta eftersom strängar är ett oföränderligt dataobjekt i Javascript. Det betyder att du inte kan ändra en del av en sträng, utan istället måste du skapa en ny sträng med det önskade innehållet.

Du kan också använda olika metoder för att manipulera och formatera strängar innan du sammanfogar dem. Till exempel kan du använda "toLowerCase()" för att göra alla bokstäver i en sträng små, eller "toUpperCase()" för att göra dem stora.

## Se även:
- [MDN webbdokumentation: String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [W3Schools: JavaScript String concatenation](https://www.w3schools.com/js/js_string_concat.asp)
- [Stack Overflow: How to concat strings in JavaScript](https://stackoverflow.com/questions/18049715/javascript-how-to-concatenate-string-random-string-random-string)