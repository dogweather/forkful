---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:03.770739-07:00
description: "\xC5 hente den n\xE5v\xE6rende datoen i TypeScript, et spr\xE5k bygget\
  \ p\xE5 JavaScript, lar deg f\xE5 tilgang til og manipulere informasjon om den n\xE5\
  v\xE6rende datoen og\u2026"
lastmod: '2024-03-11T00:14:14.073973-06:00'
model: gpt-4-0125-preview
summary: "\xC5 hente den n\xE5v\xE6rende datoen i TypeScript, et spr\xE5k bygget p\xE5\
  \ JavaScript, lar deg f\xE5 tilgang til og manipulere informasjon om den n\xE5v\xE6\
  rende datoen og\u2026"
title: "F\xE5 dagens dato"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente den nåværende datoen i TypeScript, et språk bygget på JavaScript, lar deg få tilgang til og manipulere informasjon om den nåværende datoen og tiden. Programmerere trenger ofte denne funksjonaliteten for å opprette tidsstempler, planlegging og andre tidsfølsomme funksjoner i applikasjonene sine.

## Hvordan:
I TypeScript kan du bruke `Date`-objektet for å få den nåværende datoen og tiden. Slik kan du gjøre det:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Eksempel på utdata:
```
2023-04-12T07:20:50.52Z
```

Dette kodeutsnittet lager et nytt `Date`-objekt som inneholder den nåværende datoen og tiden, som deretter skrives ut til konsollen. Du kan også formatere datoen ved å bruke toLocaleDateString() for mer lesbare formater:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Eksempel på utdata:
```
4/12/2023
```

### Bruke date-fns
For mer omfattende dato-manipulering og formatering er `date-fns`-biblioteket et populært valg. Først, installer det via npm:

```bash
npm install date-fns
```

Deretter kan du bruke det til å formatere den nåværende datoen:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Eksempel på utdata:
```
2023-04-12
```

Dette `date-fns`-eksempelet formaterer den nåværende datoen som en streng i "YYYY-MM-DD"-formatet. Biblioteket tilbyr en overflod av funksjoner for datomanipulering, noe som gjør det til et allsidig verktøy for enhver TypeScript-programmerer som jobber med datoer.
