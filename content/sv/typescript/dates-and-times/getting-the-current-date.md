---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:04.644256-07:00
description: "Hur man g\xF6r: I TypeScript kan du anv\xE4nda `Date`-objektet f\xF6\
  r att f\xE5 det aktuella datumet och tiden. S\xE5 h\xE4r kan du g\xF6ra det."
lastmod: '2024-03-13T22:44:37.666445-06:00'
model: gpt-4-0125-preview
summary: "I TypeScript kan du anv\xE4nda `Date`-objektet f\xF6r att f\xE5 det aktuella\
  \ datumet och tiden."
title: "F\xE5 det aktuella datumet"
weight: 29
---

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
