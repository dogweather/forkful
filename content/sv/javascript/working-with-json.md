---
title:                "Att arbeta med json"
html_title:           "Javascript: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-json.md"
---

{{< edit_this_page >}}

# Varför

JSON (Javascript Object Notation) är ett vanligt format för att utbyta och spara data i webbutveckling. Det är lättläst för både människor och datorer, vilket gör det till ett populärt val för att hantera data i webbapplikationer och API:er. Att lära sig hur man arbetar med JSON kan hjälpa dig att förstå hur data skickas och tas emot på internet.

## Hur man gör

Att arbeta med JSON är relativt enkelt i Javascript. För att använda JSON måste du först konvertera ett Javascript-objekt till JSON-format, använda JSON.stringify() funktionen. Här är ett exempel:

```Javascript
let person = {
  name: "Anna",
  age: 25,
  city: "Stockholm"
};

let personJson = JSON.stringify(person);
```

I det här exemplet konverterar vi ett Javascript-objekt som representerar en person till JSON-format och sparar sedan resultatet i en variabel. Nu kan vi skicka personJson variabeln över internet och den kommer att visas som JSON-data på mottagarsidan. För att konvertera tillbaka till ett Javascript-objekt, använd JSON.parse() funktionen. Här är ett annat exempel:

```Javascript
let personObj = JSON.parse(personJson);
console.log(personObj.name); // output: Anna
```

Detta exempel visar hur vi kan konvertera JSON-data till ett Javascript-objekt igen och komma åt dess värden genom att använda det angivna nyckelordet.

## Djupdykning

JSON stöder flera olika datatyper, inklusive strängar, nummer, boolean, arrayer och objekt. Det tillåter också nästlade objekt och arrayer, vilket gör det möjligt att skapa mer komplexa datastrukturer. Det är dock viktigt att vara medveten om att egenskaperna i ett JSON-objekt måste vara omgivna av citatmarkeringar, annars kommer konverteringen att misslyckas. En annan viktig aspekt att tänka på är att JSON inte stöder kommentarer, så du måste se till att ta bort eventuella kommentarer från din data innan du konverterar den.

JSON är också fördelaktigt eftersom det är plattformsoberoende, vilket innebär att det fungerar på alla moderna webbläsare och även på server-sidan med hjälp av Node.js. Det är också lätt att läsa och skriva, vilket gör det enkelt att arbeta med JSON-data även för nybörjare.

## Se även

- [JSON.org](https://www.json.org/json-en.html)
- [MDN - JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [W3Schools - JSON](https://www.w3schools.com/js/js_json_intro.asp)