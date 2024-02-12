---
title:                "Att använda associativa arrayer"
aliases:
- /sv/typescript/using-associative-arrays/
date:                  2024-01-30T19:13:22.573258-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda associativa arrayer"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & varför?

Associativa arrayer, eller objekt i TypeScript, låter dig använda strängar (eller nycklar) för att komma åt värdepar. Programmerare använder dem för mer dynamiska dataåtkomstmönster jämfört med traditionella arrayer, vilket ger ett flexibelt sätt att strukturera och komma åt data utan att vara bundna till numeriska index.

## Hur man gör:

Att skapa och använda associativa arrayer i TypeScript är enkelt. Här är en grundläggande genomgång:

```TypeScript
// Deklarera en associativ array
let user: { [key: string]: string } = {};

// Lägga till data
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

Utdata:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

Det är också lätt att iterera över nyckel-värdepar:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Utdata:

```TypeScript
name: Jane Doe
email: jane@example.com
```

Och om du hanterar en blandning av datatyper kommer TypeScripts typsystem väl till pass:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Utdata:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## Fördjupning

I TypeScript är det vi refererar till som associativa arrayer i grund och botten objekt. Historiskt sett, i språk som PHP, är associativa arrayer en grundläggande typ, men JavaScript (och därmed TypeScript) använder objekt för detta ändamål. Detta tillvägagångssätt är både en styrka och en begränsning. Objekt ger en mycket dynamisk struktur för att koppla strängar till värden, men de är inte avsedda att användas som 'arrayer' i traditionell mening. Till exempel kan du inte direkt använda arraymetoder som `push` eller `pop` på dessa objekt.

För fall där du behöver ordnade samlingar av nyckel-värdepar med array-liknande operationer, erbjuder TypeScript (och modern JavaScript) `Map`-objektet:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Även om TypeScripts typsystem och ES6-funktioner som `Map` erbjuder kraftfulla alternativ, är förståelsen av hur man använder objekt som associativa arrayer användbar för scenarier där objektliteraler är mer effektiva eller när man arbetar med JSON-datastrukturer. Det handlar allt om att välja rätt verktyg för jobbet.
