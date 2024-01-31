---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:39:08.195916-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"

category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att parsa ett datum från en sträng innebär att tolka texten och omvandla den till ett datumobjekt. Programmerare gör detta för att enkelt hantera och manipulera datumen inom deras applikationer, ofta för att interagera med databaser eller användargränssnitt.

## Hur gör man:

```TypeScript
// Grundläggande datum-parsing med JavaScripts inbyggda Date objekt.
let dateString = '2023-04-05T15:00:00.000Z';
let parsedDate = new Date(dateString);
console.log(parsedDate); // output: 2023-04-05T15:00:00.000Z

// Använda biblioteket date-fns för mer robust datum-parsing.
import { parseISO } from 'date-fns';

let parsedDateFns = parseISO(dateString);
console.log(parsedDateFns); // output: 2023-04-05T15:00:00.000Z
```

Observera att `Date` i JavaScript och TypeScript kan parse ISO 8601-format direkt men kan variera beroende på webbläsare och tidszoner.

## Djupdykning:

Förr i tiden handsköt programmerare datum-parsing, vilket ledde till buggar och inkonsekvens. Idag föredras ofta bibliotek som `date-fns` eller `moment.js` för deras robusthet och tidszonsmedvetenhet.

Det finns flera sätt att parse ett datum:

1. **Inbyggda Date-konstruktören**: Snabbt och enkelt men med begränsningar speciellt gällande tidszoner och format.
2. **RegExp**: Skapa egna regler för att extrahera datumdelar från strängar. Kräver mer arbete och är svårare att underhålla.
3. **Datumhandläggande bibliotek**: Bibliotek som `moment.js`, `date-fns`, och `Day.js` ger omfattande stöd för parsing och formatering förutom tidszoneshantering.

Till implementationsdetaljer, `date-fns` funktion `parseISO` hanterar enbart strängar i ISO-format, medan `moment` är mer förlåtande och kan hantera flera olika datumsträng-formater.

## Se även:

- MDN Web Docs för Date object: [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- `date-fns` dokumentation: [date-fns Documentation](https://date-fns.org/)
- Moment.js officiella webbplats: [Moment.js](https://momentjs.com/)
- ISO 8601-standard: [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html)
