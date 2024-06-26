---
date: 2024-01-20 17:55:12.435589-07:00
description: "How to: TypeScript bruker Node.js-funksjoner for filh\xE5ndtering. F\xF8\
  rst, install\xE9r `fs`-modulen og `@types/node` for TypeScript-typedefinisjoner."
lastmod: '2024-04-05T21:53:41.526855-06:00'
model: gpt-4-1106-preview
summary: "TypeScript bruker Node.js-funksjoner for filh\xE5ndtering."
title: Lese en tekstfil
weight: 22
---

## How to:
"## Slik gjør du:"

TypeScript bruker Node.js-funksjoner for filhåndtering. Først, installér `fs`-modulen og `@types/node` for TypeScript-typedefinisjoner:

```bash
npm install @types/node
```

Her er et eksempel på hvordan lese en tekstfil synkront:

```typescript
import { readFileSync } from 'fs';

try {
  const data = readFileSync('/sti/til/fil.txt', 'utf8');
  console.log(data);
} catch (err) {
  console.error(err);
}
```

Eller asynkront med `promises`:

```typescript
import { promises as fs } from 'fs';

fs.readFile('/sti/til/fil.txt', 'utf8')
  .then(data => console.log(data))
  .catch(error => console.error(error));
```

Sample output:

```plaintext
Dette er innholdet i filen.txt.
```

## Deep Dive:
"## Dypdykk:"

I tidlige dager av programmering var filhåndtering mer kompleks og avhengig av lavnivå-språk. Med moderne JavaScript og TypeScript, er `fs`-modulen i Node.js standard for filoperasjoner.

Alternativer til `fs` innebærer tredjepartsbiblioteker som `fs-extra` som tilbyr ekstra funksjoner.

Når du arbeider med store filer, kan strømmer (`streams`) være mer effektivt for å håndtere data sekvensielt i stedet for å laste inn hele filen i minnet.

## See Also:
"## Se også:"

- Node.js `fs` dokumentasjon: [nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- `fs-extra` bibliotek: [github.com/jprichardson/node-fs-extra](https://github.com/jprichardson/node-fs-extra)
- Om strømmer i Node.js: [nodejs.org/api/stream.html](https://nodejs.org/api/stream.html)
- TypeScript offisiell side: [typescriptlang.org](https://www.typescriptlang.org/)
