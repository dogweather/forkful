---
title:    "TypeScript: Skriver till standardfel"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför skriva till standardfelutmatning?

Att kunna skriva till standardfelutmatning i din TypeScript-kod kan vara avgörande för felsökning och optimering av ditt program. Det är ett snabbt och enkelt sätt att identifiera och spåra eventuella fel eller avvikelser i din kod. Så, varför bör du lägga till det i din programmeringsvana?

## Hur man skriver till standardfelutmatning

Skrivning till standardfelutmatning kan göras genom att använda den inbyggda funktionen "console.error()" i TypeScript. Det tar in ett argument i form av en sträng eller ett objekt som du vill skriva till standardfelutmatning. Här är ett exempel:

```TypeScript
console.error("Detta är ett felmeddelande!");
```

Detta kommer att skriva ut "Detta är ett felmeddelande!" på din standardfelutmatning. Om du vill skriva ut ett objekt kan du använda JSON.stringify() för att konvertera det till en sträng som console.error() kan hantera:

```TypeScript
let person = { name: "Anna", age: 25 };
console.error("Personens information: " + JSON.stringify(person));
```

Detta kommer att skriva ut "Personens information: {"name":"Anna","age":25}" i din standardfelutmatning. Du kan också använda console.error() för att skriva ut variabler eller funktioner som kan hjälpa till att identifiera problem i din kod.

## En djupdykning i skrivning till standardfelutmatning

Skrivning till standardfelutmatning är en viktig del av felsökning och optimering av din kod. När ett fel inträffar i din kod, kan console.error() hjälpa till att identifiera var det har skett och hjälpa till att hitta en lösning. Det är också användbart för att spåra eventuella misstag eller buggar i din kod och göra det lättare att hitta dem och fixa dem.

En annan viktig användning av skrivning till standardfelutmatning är att fånga och logga undantag. Ofta när ett undantag kastas i din kod, har du inte en aning om varför eller var det har hänt. Genom att använda console.error(), kan du fånga och logga undantaget tillsammans med annan användbar information, som till exempel variabler eller funktioner som utfördes, för att spåra problemet.

## Se även

- [Officiell TypeScript-dokumentation om skrivning till standardfelutmatning](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html)
- [Mer om hur man använder console.error()](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [En guide till felsökning i TypeScript](https://www.digitalocean.com/community/tutorials/how-to-debug-node-js-with-the-built-in-debugger-and-vscode)