---
title:                "Javascript: Att extrahera delsträngar"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar är en viktig del av Javascript-programmering eftersom det gör det möjligt att manipulera och anpassa textsträngar på ett effektivt sätt. Genom att kunna välja specifika delar av en textsträng kan du göra avancerade sökningar och ersättningar eller helt enkelt bara göra texten mer läsbar för användaren.

## Hur man gör det

För att extrahera substrängar i Javascript finns det flera inbyggda metoder som kan användas. Den vanligaste metoden är att använda sig av `substring()` funktionen. Detta tar två parametrar, start- och slutindex, och returnerar den del av textsträngen som ligger mellan dessa index. Till exempel:

```Javascript
let text = "Jag älskar Javascript!";
let extractedText = text.substring(7, 16);
console.log(extractedText); // "Javascript"
```

Du kan också använda `slice()` funktionen på samma sätt. Det som skiljer `slice()` från `substring()` är att `slice()` kan hantera negativa index, vilket gör det enklare att extrahera text från slutet av en sträng. Till exempel:

```Javascript
let text = "Jag älskar Javascript!";
let extractedText = text.slice(-10);
console.log(extractedText); // "Javascript"
```

En annan användbar metod är `substr()` som tar två parametrar, startindex och antalet tecken att extrahera. Detta är särskilt användbart när du inte känner till längden på den del du vill extrahera. Till exempel:

```Javascript
let text = "Jag älskar Javascript!";
let extractedText = text.substr(7, 10);
console.log(extractedText); // "Javascript"
```

## Djupdykning

När du arbetar med substrängar är det viktigt att förstå skillnaden mellan `substring()` och `slice()` i vissa fall. Till exempel, om du försöker använda negativa index på `substring()` får du ett annat resultat än om du använder `slice()`. Dessutom finns det en subtil skillnad mellan `substring()` och `substr()` när det kommer till hur de hanterar sina parametrar - `substring()` tar emot två index, medan `substr()` tar ett startindex och ett antal tecken att extrahera.

En annan intressant sak att notera är att när du ger en negativ längd till `substr()` som den andra parameter, kommer den att behandla det som ett startindex och extrahera text från det indexet till slutet av strängen.

## Se också

- [MDN - substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN - slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN - substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)