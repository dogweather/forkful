---
title:                "Analysera ett datum från en sträng"
aliases:
- /sv/typescript/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:40.206009-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysera ett datum från en sträng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad och varför?
Att tolka ett datum från en sträng innebär att konvertera textuella representationer av datum och tider till ett format som kan manipuleras och analyseras av programmet. Detta är en vanlig uppgift i programmering eftersom det möjliggör hantering av användarinmatning, lagring av tidsstämplad data och interaktioner med API:er, vilket ger mer funktionella och användarvänliga applikationer.

## Hur man gör:
TypeScript, som är en övermängd av JavaScript, förlitar sig på Date-objektet för att tolka datum från strängar. Att arbeta med datum i JS/TS kan dock bli omständligt eller oexakt på grund av Date-objektets egenheter. Här är ett grundläggande exempel följt av en metod med hjälp av det populära biblioteket `date-fns` för mer robusta lösningar.

### Använda JavaScripts Date-objekt
```typescript
// Grundläggande tolkning med Date-konstruktören
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// Utdata för GMT: "Fri Apr 21 2023 15:00:00 GMT+0000 (Coordinated Universal Time)"
```

Denna metod fungerar för ISO-formatsträngar och vissa andra datumformat, men kan ge inkonsekventa resultat för tvetydiga format över webbläsare och lokaler.

### Använda date-fns
Biblioteket `date-fns` erbjuder enkel och konsekvent hantering av datum. Det är ett modulärt bibliotek, vilket tillåter dig att inkludera endast de delar du behöver, vilket minskar paketstorleken.

Först, installera `date-fns`:

```sh
npm install date-fns
```

Använd det sedan för att tolka en datumsträng:

```typescript
import { parseISO, format } from 'date-fns';

// Tolkar en ISO-sträng
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// Formatera datumet (t.ex. till ett läsbart format för människor)
console.log(format(parsedDate, "PPPpp")); 
// Utdata: "Apr 21st, 2023 at 3:00 PM" (utdata kan variera baserat på lokal)
```

`date-fns` stöder ett brett utbud av format och lokaler, vilket gör det till ett robust val för applikationer som kräver exakt datumtolkning och formatering över olika användarregioner.
