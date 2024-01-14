---
title:                "TypeScript: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON har blivit en viktig del av webbutveckling och kan användas för att överföra data mellan en klient och en server. Genom att arbeta med JSON, kan man enkelt strukturera och hantera data på ett sätt som är läsbart för både människor och datorer. För att bli en framgångsrik webbutvecklare är det därför viktigt att förstå hur man arbetar med JSON.

## Hur man jobbar med JSON i TypeScript

För att börja arbeta med JSON i TypeScript, behöver man bara använda det inbyggda "JSON" objektet som tillhandahåller metoder för att konvertera data till och från JSON-format.

Först måste vi skapa ett objekt som vi vill konvertera till JSON-format. I exemplet nedan skapar vi ett användarobjekt med namn, ålder och e-postadress:

```TypeScript
let user = {
  name: "Johan Svensson",
  age: 25,
  email: "johan.svensson@example.com"
};
```

För att konvertera detta objekt till JSON, kan vi använda "JSON.stringify()" metoden och tilldela resultatet till en variabel:

```TypeScript
let userJSON = JSON.stringify(user);
```

Nu har vi en variabel som innehåller användarobjektet i JSON-format och kan skicka den till en server eller använda den på andra sätt.

Om vi vill konvertera JSON tillbaka till ett objekt, kan vi använda "JSON.parse()" metoden:

```TypeScript
let userObject = JSON.parse(userJSON);
```

I detta exempel får vi tillbaka samma användarobjekt som vi skapade i början.

## Djupdykning i JSON

JSON stöder flera olika datatyper, inklusive objekt och arrayer. Det är också möjligt att bädda in objekt inuti andra objekt, vilket kan vara användbart när man hanterar komplexa datamodeller.

Vid konvertering från ett objekt till JSON, är det viktigt att notera att alla funktioner (metoder) tas bort. Detta kan skapa problem om man försöker att konvertera tillbaka till ett objekt, eftersom funktionerna kommer att vara borta. En möjlig lösning på detta problem är att använda alternativa sätt att konvertera data till JSON, till exempel genom att använda bibliotek som "JSON.stringify()" med funktioner.

## Se även

- Lär dig mer om JSON på [MDN webbdokumentation](https://developer.mozilla.org/sv/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- Hitta en lista över bibliotek som stöder funktioner när man konverterar till JSON på [GitHub](https://github.com/tc39/proposal-staged-json-stringify)