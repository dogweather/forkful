---
title:                "TypeScript: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Varför

Att söka och ersätta text är en vanlig uppgift inom programmering. Oavsett om du behöver ändra namn på variabler eller ersätta felaktig text, kan det vara en tidsbesparande teknik att behärska.

# Såhär gör du

I TypeScript finns det olika metoder för att söka och ersätta text. En av de vanligaste är `replace()`-metoden, som tar två argument: en söksträng och en sträng som ska ersätta den hittade texten.

```TypeScript
let text = "Hej världen!";
let nyText = text.replace("världen", "du");
console.log(nyText);

// Output: Hej du!
```

För att göra en global sökning och ersättning, lägg till en `g`-flagga som sista argumentet i `replace()`-metoden. Detta gör att alla förekomster av söksträngen kommer att ersättas.

```TypeScript
let text = "Välkommen till TypeScript!";
let nyText = text.replace(/o/g, "a");
console.log(nyText);

// Output: Välkammen till TypeScrapt!
```

# Djupdykning

Utöver `replace()`-metoden finns det andra sätt att söka och ersätta text i TypeScript. Till exempel kan du använda `indexOf()`-metoden för att hitta positionen för en specifik teckenföljd i en sträng och sedan använda `slice()`-metoden för att ersätta den.

```TypeScript
let text = "Hello world!";
let position = text.indexOf("world");
let nyText = text.slice(0, position) + "universe";
console.log(nyText);

// Output: Hello universe!
```

Det finns också regex-mönster som kan användas för att söka efter specifika mönster i texten och sedan ersätta dem. Detta ger en mer avancerad och flexibel sökning och ersättning.

# Se även

- [TypeScript officiell dokumentation om strängar](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [W3Schools guide om sökning och ersättning i JavaScript](https://www.w3schools.com/jsref/jsref_replace.asp)
- [MDN webbdokumentation om regex i JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)