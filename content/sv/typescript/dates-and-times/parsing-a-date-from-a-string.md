---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:40.206009-07:00
description: "Hur man g\xF6r: TypeScript, som \xE4r en \xF6verm\xE4ngd av JavaScript,\
  \ f\xF6rlitar sig p\xE5 Date-objektet f\xF6r att tolka datum fr\xE5n str\xE4ngar.\
  \ Att arbeta med datum i JS/TS\u2026"
lastmod: '2024-03-13T22:44:37.665315-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, som \xE4r en \xF6verm\xE4ngd av JavaScript, f\xF6rlitar sig\
  \ p\xE5 Date-objektet f\xF6r att tolka datum fr\xE5n str\xE4ngar."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

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
