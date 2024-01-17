---
title:                "Arbeta med json"
html_title:           "Javascript: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med JSON innebär att man hanterar data i ett format som är lätt att parsa och läsbart för både människor och maskiner. Det är en populär metod bland programmerare för att skicka och ta emot data på ett standardiserat sätt.

## Hur man gör:

Skapa en variabel som innehåller ett objekt med olika attribut. Använd JSON.stringify() för att konvertera objektet till en sträng i JSON-format. För att läsa ett JSON-dokument behöver du använda JSON.parse() för att konvertera strängen tillbaka till ett JavaScript-objekt.

```Javascript
let user = {
  name: "John Doe",
  age: 28,
  hobbies: ["reading", "coding", "hiking"]
};

// Konvertera objektet till en sträng i JSON-format
let userJSON = JSON.stringify(user);

// Konvertera tillbaka till ett JavaScript-objekt
let newUser = JSON.parse(userJSON);
```

Output: ```Javascript {"name": "John Doe", "age": 28, "hobbies": ["reading", "coding", "hiking"]}```
 
## Djupdykning:

JSON står för JavaScript Object Notation och har funnits sedan 2001. Det är ett lättviktigt och självdokumenterat format som bygger på JavaScript-syntax. Alternativ till JSON är bland annat XML och CSV, men JSON har blivit mer populärt på grund av sitt enkla och läsbara format.

## Se även:

https://www.json.org/ - Officiell JSON-hemsida med information och dokumentation.
https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON - Information och exempel om att arbeta med JSON i JavaScript från Mozilla.