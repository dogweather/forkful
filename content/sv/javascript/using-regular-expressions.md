---
title:                "Använda reguljära uttryck"
html_title:           "Javascript: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

#Vad & Varför?
Regular expressions, eller regex, är ett sätt att söka efter mönster i textsträngar. Det är ett kraftfullt verktyg som programmörer använder för att hitta och manipulera data i texter. Genom att använda regex kan vi snabbt och effektivt extrahera information från stora mängder text.

#Hur man gör:
För att använda regex i JavaScript använder vi objektet "RegExp". Här är ett enkelt exempel på hur man hittar matchningar av tecknet "a" i en textsträng:

```JavaScript
let regex = /a/g; 
let text = "JavaScript är ett kraftfullt verktyg för webbutveckling."; 

let result = text.match(regex); // resultatet blir en array med alla matchningar
console.log(result); // ["a","a","a","a"];
```

Vi kan också använda regex för att ersätta delar av en textsträng. I exemplet nedan byter vi ut alla siffror i en textsträng till ett annat tecken:

```JavaScript
let regex = /[0-9]/g; 
let text = "Denna text innehåller både siffror (123) och bokstäver."; 

let result = text.replace(regex, "*"); // resultatet blir "Denna text innehåller både siffror (*) och bokstäver."
```

#Djupdykning:
Reguljära uttryck har funnits sedan 1950-talet och har använts inom många olika programmeringsspråk. I JavaScript används de främst för att söka, matcha och ersätta texter. En annan populär metod för strängmanipulering är "string methods", men regex erbjuder ett mer kraftfullt och flexibelt sätt att hantera texter.

Alternativ till regex är "wildcard patterns" och "string searching algorithms", men dessa är oftast mindre precisa och mer begränsade i funktionalitet.

Det finns också vissa detaljer att tänka på när man använder regex i JavaScript. Till exempel måste man lägga till "\\" exemplet '/\w/' för att matcha ett tecken som börjar med '\\'. Det finns också några skillnader i hur regex fungerar i olika webbläsare, så det är viktigt att testa koden i olika miljöer.

#Se också:
För mer information om användning av reguljära uttryck i JavaScript kan du kolla in följande resurser:

- MDN docs: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
- W3Schools: https://www.w3schools.com/js/js_regexp.asp