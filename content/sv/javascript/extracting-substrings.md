---
title:    "Javascript: Extrahering av substränger"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

I Javascript, liksom i många andra programmeringsspråk, finns det en funktion för att extrahera en delsträng från en befintlig sträng. Detta är användbart när man vill manipulera eller förändra en del av en större textsträng.

## Hur du gör det

För att extrahera en delsträng i Javascript använder man metoden `substring()` och anger start- och slutindex för den delsträng man vill få ut. Till exempel:

```Javascript
var str = "Hej, det här är en textsträng.";
var delstrang = str.substring(4, 11);

console.log(delstrang); // Output: det här
```

Här extraherar vi delsträngen "det här" från den ursprungliga strängen baserat på dess indexposition. Det är viktigt att komma ihåg att indexpositionen börjar på 0, vilket betyder att första tecknet i en sträng har indexposition 0.

I vissa fall kanske du inte känner till exakta indexpositioner och istället vill extrahera delsträngar baserat på ett visst mönster. I ett sådant fall kan man använda metoden `substr()` som tar in en startposition och antal tecken som argument. Se nedanstående exempel:

```Javascript
var str = "Välkommen till min blogg!";
var delstrang = str.substr(10, 9);

console.log(delstrang); // Output: under min
```

Här extraherade vi delsträngen "under min" från den ursprungliga strängen baserat på mönstret av 9 tecken som börjar vid indexposition 10.

## Djupdykning

Förutom `substring()` och `substr()` finns det också en metod som heter `slice()` som också kan användas för att extrahera delsträngar. Syntaxen är liknande som för `substring()` men `slice()` kan också ta in negativa index vilket är användbart om man vill extrahera delsträngar bakifrån.

En annan användbar funktion i samband med delsträngar är `indexOf()` som returnerar indexpositionen för en viss text inom en sträng. Detta kan vara användbart för att hitta start- och slutindex för en delsträng för att sedan kunna använda `substring()` för att extrahera den.

## Se även

- [MDN - substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN - substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN - slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN - indexOf()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf)