---
title:                "Få det aktuella datumet"
aliases:
- sv/typescript/getting-the-current-date.md
date:                  2024-02-03T19:11:04.644256-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få det aktuella datumet"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att få det aktuella datumet i TypeScript, ett språk byggt på JavaScript, gör det möjligt för dig att komma åt och manipulera nuvarande datum- och tidsinformation. Programmerare behöver ofta denna funktionalitet för att skapa tidsstämplar, schemaläggning och andra tidskänsliga funktioner i sina applikationer.

## Hur man gör:
I TypeScript kan du använda `Date`-objektet för att få det aktuella datumet och tiden. Så här kan du göra det:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Exempelutskrift:
```
2023-04-12T07:20:50.52Z
```

Denna kodsnutt skapar ett nytt `Date`-objekt som innehåller det aktuella datumet och tiden, som sedan skrivs ut till konsolen. Du kan också formatera datumet med toLocaleDateString() för mer läsliga format:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Exempelutskrift:
```
2023-04-12
```

### Använda date-fns
För mer omfattande datummanipulering och formatering är `date-fns`-biblioteket ett populärt val. Installera det först via npm:

```bash
npm install date-fns
```

Sedan kan du använda det för att formatera det aktuella datumet:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Exempelutskrift:
```
2023-04-12
```

Detta `date-fns`-exempel formaterar det aktuella datumet som en sträng i formatet "ÅÅÅÅ-MM-DD". Biblioteket erbjuder en mängd funktioner för datummanipulering, vilket gör det till ett mångsidigt verktyg för varje TypeScript-programmerare som arbetar med datum.
