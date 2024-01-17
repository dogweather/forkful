---
title:                "Sammanfogande av strängar"
html_title:           "Javascript: Sammanfogande av strängar"
simple_title:         "Sammanfogande av strängar"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Konkatenering av strängar är en vanlig teknik inom programmering där man kombinerar flera individuella strängar till en enda sträng. Detta görs oftast för att skapa en mer läsbar och användbar sträng, till exempel för att skriva ut text på en webbsida eller för att skicka meddelanden till användare.

## Så här gör du:

```Javascript
// Skapa två variabler med strängar
var namn = "Anna";
var hälsning = "Hej!";

// Kombinera strängarna med hjälp av + operatorn
var meddelande = namn + " " + hälsning;

// Skriv ut resultatet
console.log(meddelande); // Utmatning: "Anna Hej!"
```

## Djupdykning:

Konkatenering av strängar har funnits sedan de tidiga dagarna av programmering, då man använde en metod som kallades för "string interpolation". Detta involverade att man manuellt placerade samman individuella strängar och mellanslag för att skapa en komplett sträng. Idag finns det dock många moderna metoder i språk som Javascript som gör konkatenering enklare och mer effektiv.

En annan alternativ metod för att kombinera strängar är template literals, som introducerades i ES6. Istället för att använda + operatorn, kan man använda backticks och variabelnamn inuti strängen för att dynamiskt bygga en sträng.

När det gäller implementation så är Javascript en dynamiskt typat språk, vilket innebär att man inte behöver specificera datatyp för variabler. Det betyder att man kan konkatenera strängar med andra datatyper, som nummer eller booleska värden, utan problem.

## Se också:

Mer information och exempel på konkatenering av strängar i Javascript: https://www.w3schools.com/jsref/jsref_concat_string.asp