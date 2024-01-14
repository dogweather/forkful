---
title:    "Javascript: Extrahera substrängar"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Ibland när vi arbetar med textsträngar i våra JavaScript-program vill vi endast extrahera en del av strängen, istället för hela strängen. Det kan bero på att vi behöver ett specifikt ord eller en del av en datingsträng. Genom att lära oss hur man extraherar substrängar kan vi göra våra program mer flexibla och kraftfulla.

## Så här gör du

Först måste vi bestämma vilken del av strängen som vi vill extrahera. Detta kan göras genom att använda antingen index eller en söksträng. Om du bestämmer dig för att använda index, tänk på att den första bokstaven i en sträng har indexet 0.

```Javascript
const str = "Hej detta är en sträng."
console.log(str.substring(4,7)) // Output: detta
console.log(str.substring(5)) // Output:  är en sträng.
console.log(str.slice(-9, -1)) // Output: n sträng
console.log(str.substr(4)) // Output: detta är en sträng. 
``` 

Du kan också använda en söksträng för att extrahera en del av en sträng. I exemplet nedan använder vi `indexOf()` för att hitta indexet för den sökta texten, vilket sedan används i `substring()`.

```Javascript
const str = "Lorem ipsum dolor sit amet"
const search = "dolor"
const index = str.indexOf(search)
console.log(str.substring(index, index+search.length)) // Output: dolor
```

För att göra det ännu enklare finns det också metoder som `includes()`, `startsWith()` och `endsWith()` som kan användas tillsammans med `substring()` för att extrahera delar av en sträng baserat på kompletterande villkor.

## Djupdykning

Det är också möjligt att extrahera delar av en sträng baserat på ett regex-mönster. `match()`-metoden kan användas för att matcha texten mot ett regex-mönster och returnera en array med matchande delar av strängen.

```Javascript
const str = "Detta är en text med en specifik del som vi vill extrahera."
const regex = /specifik del/
const match = str.match(regex)
console.log(match[0]) // Output: specifik del
```

Vi kan också använda en modifierare för att göra matchningen global eller fallkänslig.

```Javascript
const str = "Detta är Testtext"
console.log(str.match(/test/gi)) // Output: Test
```

## Se även

- [substr() - MDN webbdokumentation](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [substring() - MDN webbdokumentation](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [match() - MDN webbdokumentation](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/String/match)