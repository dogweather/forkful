---
title:                "Ottenere la data corrente"
date:                  2024-02-03T19:11:10.527411-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ottenere la data corrente"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Ottenere la data corrente in TypeScript, un linguaggio basato su JavaScript, consente di accedere e manipolare le informazioni sulla data e l'ora correnti. Gli sviluppatori spesso necessitano di questa funzionalità per creare timestamp, programmare e altre funzioni sensibili al tempo nelle loro applicazioni.

## Come fare:
In TypeScript, puoi usare l'oggetto `Date` per ottenere la data e l'ora correnti. Ecco come puoi farlo:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Output di esempio:
```
2023-04-12T07:20:50.52Z
```

Questo frammento di codice crea un nuovo oggetto `Date` contenente la data e l'ora correnti, che viene poi stampato sulla console. Puoi anche formattare la data usando toLocaleDateString() per formati più leggibili:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Output di esempio:
```
12/4/2023
```

### Usando date-fns
Per una manipolazione e formattazione della data più estesa, la libreria `date-fns` è una scelta popolare. Prima, installala tramite npm:

```bash
npm install date-fns
```

Poi, puoi usarla per formattare la data corrente:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Output di esempio:
```
2023-04-12
```

Questo esempio di `date-fns` formatta la data corrente come stringa nel formato "YYYY-MM-DD". La libreria offre una pletora di funzioni per la manipolazione delle date, rendendola uno strumento versatile per qualsiasi programmatore TypeScript che lavori con le date.
