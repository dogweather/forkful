---
title:                "Javascript: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med JSON i Javascript kan vara en användbar färdighet för utvecklare som vill hantera data på ett effektivt, strukturerat och läsbart sätt. JSON, som står för JavaScript Object Notion, är ett populärt format för att lagra och överföra data mellan applikationer och webbplatser.

## Hur man gör

För att skapa och använda JSON i Javascript behöver du först förstå dess syntax. JSON är i grunden en textbaserad datastruktur som består av ett antal värden och nycklar. Värdena kan vara av olika typer, som strängar, tal eller booleska värden, medan nycklarna är de namn som refererar till varje värde.

För att skapa en JSON-sträng kan du använda formatet ```JSON.stringify()``` och ange dina värden och nycklar som parametrar. Till exempel:

```Javascript
let obj = {
  name: "Lisa",
  age: 25, 
  city: "Stockholm"
};

let jsonStr = JSON.stringify(obj);
console.log(jsonStr);
```

Detta kommer att ge följande output:

```
{"name":"Lisa","age":25,"city":"Stockholm"}
```

För att sedan kunna använda JSON-data i ditt Javascript-program kan du använda ```JSON.parse()``` för att omvandla den till ett vanligt Javascript-objekt. Till exempel:

```Javascript
let jsonObj = JSON.parse(jsonStr);
console.log(jsonObj.name); // Output: Lisa
```

På liknande sätt kan du också lägga till, uppdatera eller ta bort värden från ditt JSON-objekt genom att använda dess nycklar.

## Djupdykning

Det finns många andra funktioner och metoder som du kan använda dig av när du arbetar med JSON i Javascript. Till exempel kan du använda ```JSON.stringify()``` med en annan parameter, ```replacer```, för att välja vilka värden som ska läggas till i din JSON-sträng. Du kan också använda ```JSON.parse()``` med argumentet ```reviver``` för att direkt konvertera din JSON-data till ett Javascript-objekt med önskad typ.

Det är också viktigt att känna till att JSON inte tillåter värden av typen undefined och det kan orsaka felaktigheter om du inte hanterar det korrekt. Du kan använda ```undefined``` för att ta bort en egenskap från ditt JSON-objekt, men det kommer att resultera i att egenskapen inte finns med alls i din resulterande JSON-sträng.

## Se också

- [MDN - JSON](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [W3Schools - JSON Introduction](https://www.w3schools.com/js/js_json_intro.asp)
- [JavaScript.info - JSON](https://javascript.info/json)