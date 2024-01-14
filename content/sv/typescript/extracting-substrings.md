---
title:                "TypeScript: Uttagna delsträngar"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera delsträngar från en större sträng är en vanlig uppgift inom programmering, särskilt när man arbetar med textbaserade data eller behöver bearbeta användaringångar. Genom att kunna extrahera specifika delar av en sträng kan man effektivisera sin kod och få tillgång till viktig information på ett snabbt och enkelt sätt.

## Hur man gör det

För att extrahera en delsträng från en större sträng i TypeScript använder man "slice()" metoden. Denna metod tar två parametrar, startindex och slutindex, och returnerar en ny sträng som endast innehåller tecken mellan de angivna indexen (inklusive startindex men exklusive slutindex).

```TypeScript
let sträng = "Detta är en textsträng.";

// Extrahera "en"
let delsträng = sträng.slice(11, 14);
console.log(delsträng); // "en"

// Extrahera sista ordet
delsträng = sträng.slice(18);
console.log(delsträng); // "textsträng."
```

För att undvika att ange slutindexet kan man istället ange ett negativt tal som motsvarar antal tecken från slutet av strängen. Detta är särskilt användbart när man vill hämta en del av en sträng som ligger långt in i strängen.

```TypeScript
// Hämta sista fyra tecknen
delsträng = sträng.slice(-4);
console.log(delsträng); // "äng."
```

## Djupdykning

Utöver "slice()" metoden finns det även andra sätt att extrahera delsträngar i TypeScript. Man kan använda "substring()" metoden som fungerar på liknande sätt men tar istället två index som parametrar och returnerar en sträng mellan dessa index (inklusive båda).

```TypeScript
// Extrahera "är en"
delsträng = sträng.substring(5, 10);
console.log(delsträng); // "är en"
```

En annan metod är "substr()" som tar två parametrar, startindex och antal tecken, och returnerar en sträng med det angivna antalet tecken från och med startindexet.

```TypeScript
// Hämta de tre första tecknen
delsträng = sträng.substr(0, 3);
console.log(delsträng); // "Det"
```

Slutligen kan man även använda en kombination av "indexOf()" och "substring()" metoden för att extrahera specifika delar av en sträng baserat på förekomsten av ett visst tecken eller en viss sträng.

Se mer om dessa metoder och deras användning i den officiella dokumentationen för TypeScript.

## Se även

- [`slice()` metod i TypeScript dokumentationen](https://www.typescriptlang.org/docs/handbook/strings.html#the-substring-method)
- [`substring()` metod i TypeScript dokumentationen](https://www.typescriptlang.org/docs/handbook/strings.html#the-substring-method)
- [`substr()` metod i TypeScript dokumentationen](https://www.typescriptlang.org/docs/handbook/strings.html#the-substr-method)
- [`indexOf()` metod i TypeScript dokumentationen](https://www.typescriptlang.org/docs/handbook/strings.html#the-indexof-method)