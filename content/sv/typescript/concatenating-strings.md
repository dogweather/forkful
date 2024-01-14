---
title:    "TypeScript: Sammanslagning av strängar"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en vanlig uppgift i programmering och det är en viktig funktion att förstå för alla som vill bli ett proffs inom TypeScript. Genom att lära sig hur man isolerar, modifierar och sammanfogar olika textsträngar kan du skriva effektivare kod och bygga mer avancerade applikationer.

## Hur man gör

För att sammanslå strängar i TypeScript kan du använda operatorn "+" eller metoden "concat()" som finns tillgänglig på varje strängobjekt. Låt oss titta på några kodexempel för att förstå hur detta fungerar:

```TypeScript 
let str1 = "Detta ";
let str2 = "är en ";
let str3 = "sträng.";

// Sätter ihop en ny sträng genom att använda operatorn "+"
let concatenatedStr = str1 + str2 + str3;
console.log(concatenatedStr);
// Output: Detta är en sträng.

// Använder metoden "concat()"
let newStr = str1.concat(str2, str3);
console.log(newStr);
// Output: Detta är en sträng.
```

Som du kan se behöver vi bara använda "+" eller "concat()" för att lägga ihop flera strängar till en enda sträng. Du kan också sätta samman en mix av textsträngar och andra datatyper, till exempel nummer, genom att konvertera dem till strängar först.

## Djupdykning

För att sätta ihop stora mängder av strängar kan det vara mer effektivt att använda en lista istället för att upprepa operatorn "+" eller "concat()". I TypeScript finns det en inbyggd funktion som heter "join()" som låter dig göra detta enkelt. Låt oss titta på en kodexempel:

```TypeScript
let wordsList = ["Jag", "älskar", "att", "programmera"];
let newStr = wordsList.join(" ");
console.log(newStr);
// Output: Jag älskar att programmera.
```

I detta exempel tar vi en lista av ord och sätter ihop dem med hjälp av "join()" funktionen, som lägger till ett mellanslag mellan varje ord. Detta är en smidigare och mer effektiv metod än att använda "+" eller "concat()" flera gånger.

## Se också

- [MDN web docs: Concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Arithmetic_Operators#Concatenation)
- [6 Examples of String Concatenation Using TypeScript](https://dzone.com/articles/6-examples-of-string-concatenation-using-typescript)
- [TypeScript: String Concatenation Tutorial](https://appdividend.com/2019/08/22/javascript-string-concatenation-tutorial-with-example/)