---
title:                "Sammanfogning av strängar"
html_title:           "Javascript: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att kunna sammanfoga strängar är en viktig del av programmering i allmänhet och Javascript i synnerhet. Genom att kombinera flera strängar till en enda kan man skapa mer flexibla och dynamiska applikationer, vilket ökar funktionernas användbarhet och effektivitet.

## Hur

Sammanfogning av strängar i Javascript görs med hjälp av operators. Det finns två olika operators som kan användas för att sammanfoga strängar: ```+```och ```+=```.

* ```+``` används för att enkelt kombinera två eller flera strängar. Resultatet blir en helt ny sträng som innehåller alla de ursprungliga strängarna sammanslagna i samma ordning som de förekom i.
Exempel: 

```Javascript
var firstName = "Lisa";
var lastName = "Svensson";
var fullName = firstName + " " + lastName;
console.log(fullName); // Output: Lisa Svensson
```

* ```+=``` används för att sammanfoga en redan existerande variabel med en eller flera strängar. Resultatet sparas då i den ursprungliga variabeln.
Exempel:

```Javascript
var quote = "Var inte rädd för förändring, den kan leda till något bättre.";
quote += " - Madonna";
console.log(quote); // Output: Var inte rädd för förändring, den kan leda till något bättre. - Madonna
```

När man använder operators för att sammanfoga strängar är det viktigt att komma ihåg att man inte kan sammanföra strängar med andra datatyper, som t.ex. numeriska värden. Då kommer resultatet att bli en strängkonvertering av dessa värden istället.

## Deep Dive

Även om det kan verka som en enkel funktion, finns det några saker att tänka på när man sammanfogar strängar i Javascript.

* Sammanfogning av strängar med operators kan snabbt bli ineffektivt för större mängder data. I dessa fall kan det vara bättre att använda metoden ```concat()``` som är optimerad för att hantera stora mängder data.
Exempel:

```Javascript
var fullName = "Lisa Svensson".concat(" bor i Stockholm", " och är 30 år gammal");
console.log(fullName); // Output: Lisa Svensson bor i Stockholm och är 30 år gammal
```

* För att undvika att strängar oväntat konverteras till numeriska värden, är det en bra vana att alltid konvertera numeriska värden till strängar innan de läggs till i en sammanfogad sträng.
Exempel:

```Javascript
var price = 100;
var message = "Priset för denna produkt är " + String(price) + " kr.";
console.log(message); // Output: Priset för denna produkt är 100 kr.
```

## Se även

Här är några länkar för att läsa mer om sammanfogning av strängar i Javascript:

* [MDN Web Docs](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Operators/Addition_assignment)
* [W3Schools](https://www.w3schools.com/jsref/jsref_concat_string.asp)
* [JavaScript.info](https://javascript.info/string)