---
title:                "Söka och ersätta text"
html_title:           "Javascript: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Att söka och ersätta text är en viktig del av programmering eftersom det gör det möjligt för oss att göra snabba och effektiva förändringar i stora mängder kod. Det kan hjälpa till att spara tid och extrahera specifika delar av text på ett enkelt sätt.

## Hur man gör det
För att söka och ersätta text i Javascript kan du använda metoden "replace()". Här är ett exempel på hur du skulle kunna använda den för att ersätta alla förekomster av ordet "hej" med "hello":

```Javascript
let text = "Hej, välkommen! Hej, hur mår du?";
let ersattText = text.replace(/hej/g, "hello");
console.log(ersattText);

// Output: "Hello, välkommen! Hello, hur mår du?"
```

För att förstå koden ovan behöver vi bryta ner den i mindre bitar:

- *text* är en variabel som innehåller den ursprungliga texten.
- *replace()* är en inbyggd funktion i Javascript som används för att ersätta text.
- "/hej/g" är ett uttryck som söker efter alla förekomster av ordet "hej" i texten och "/g" innebär att alla förekomster i hela texten ska ersättas, inte bara den första.
- "hello" är den text som vi vill ersätta "hej" med.
- *ersattText* är en variabel som innehåller den uppdaterade texten med de nya ersättningarna.
- *console.log()* används för att skriva ut resultatet av vår kod i konsolen.

Du kan också använda denna metod för att ersätta delar av en sträng istället för hela ord. Till exempel om du vill ersätta de första tre bokstäverna i ett ord med en annan text:

```Javascript
let text = "apple, orange, banana";
let ersattText = text.replace(/app/g, "straw");
console.log(ersattText);

// Output: "strawle, orange, banana"
```

Det finns många olika sätt att använda sök- och ersättningsmetoden i Javascript, så det är viktigt att experimentera och hitta den som passar bäst för dina behov.

## Djupdykning
En annan användbar metod för att söka och ersätta text i Javascript är "replaceAll()". Denna funktion fungerar på samma sätt som "replace()" men ersätter alla förekomster av ett mönster, inte bara det första. Detta är särskilt användbart när du arbetar med längre och mer komplexa strängar.

En annan viktig aspekt att notera är att sök- och ersättningsmetoderna är fallkänsliga, vilket betyder att de skiljer mellan små och stora bokstäver. Om du vill att sökningen ska vara oberoende av bokstäver, kan du lägga till "i" efter det sista "/". Till exempel:

```Javascript
let text = "Hello, World!";
let ersattText = text.replace(/ELLO/i, "ello");
console.log(ersattText);

// Output: "hello, World!"
```

## Se även
- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools: JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [JavaScript.info: ReplaceAll](https://javascript.info/regexp-methods#replacement)