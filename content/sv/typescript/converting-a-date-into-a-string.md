---
title:                "Omvandla ett datum till en sträng"
html_title:           "Bash: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera ett datum till en sträng innebär att förändra datatypen från "Date" till en läsbar sträng. Det är användbart för att förbättra användarupplevelsen och göra data mer hanterbar.

## Hur Man:

Att omvandla ett datum till strängformat i TypeScript är rakt på sak. Använd 'toISOString' för att få en ISO 8601-sträng:

```typescript
let datum = new Date();
let datumStrang = datum.toISOString();
console.log(datumStrang); 
```

Output kommer att se ut som något i stil med `2019-02-16T16:42:00.000Z`.

Även `toLocaleString()` kan vara användbart för lokaliserade datumtidssträngar:

```typescript
let datum = new Date();
let datumStrang = datum.toLocaleString('sv-SE');
console.log(datumStrang);
```

Denna output kommer att vara i Svensk format, till exempel `2019-02-16 16:42:00`.

## Djupdykning:

Denna metod har en bred historisk användning i JavaScript, TypeScript's mor språk. Det går också att använda bibliotek som Moment.js för mer komplicerade datumsträngformatteringar.

Det är viktigt att notera att output av dessa metoder kan variera beroende på tidzonen inställningar av systemet som kör koden. Om du behöver en garanterad output, överväga ett bibliotek som Luxon som ger tillgång till datum/tid manipulation och zonhantering.

## Se Även:

För mer detaljer och alternativ för att konvertera datum till strängar, kolla in dessa länkar:

- [Date - Javascript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Internationalization (i18n) - Node.js | Node.js docs](https://nodejs.org/api/intl.html)
- [Luxon | A powerful, modern, and friendly wrapper for Javascript dates and times.](https://moment.github.io/luxon/)