---
title:                "Ottenere la data corrente"
date:                  2024-02-03T19:10:01.646444-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ottenere la data corrente"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Ottenere la data corrente in JavaScript è un compito fondamentale, che comporta il recupero e, eventualmente, la manipolazione della data e dell'ora di oggi. I programmatori effettuano questa operazione per visualizzare le date sui siti web, nelle applicazioni, per monitorare le interazioni degli utenti o per gestire dati sensibili al tempo.

## Come fare:
In JavaScript "vanilla", l'oggetto `Date` viene utilizzato per lavorare con date e orari. Ecco come si può ottenere la data e l'ora corrente:

```javascript
const currentDate = new Date();
console.log(currentDate); // Esempio di output: Fri Apr 14 2023 12:34:56 GMT+0100 (Ora Legale Britannica)
```

Per visualizzare solo la data in un formato più accessibile, si possono utilizzare metodi come `toLocaleDateString()`:

```javascript
console.log(currentDate.toLocaleDateString()); // Esempio di output: 14/4/2023
```

Per avere maggior controllo sul formato, librerie di terze parti come *Moment.js* o *date-fns* sono molto popolari, anche se è bene essere consapevoli del fatto che Moment.js è ora considerato un progetto legacy in modalità di manutenzione.

Utilizzando *Moment.js*:

```javascript
const moment = require('moment'); // assumendo Node.js o l'uso di un bundler di moduli
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Esempio di output: 2023-04-14
```

Con *date-fns*, che enfatizza la modularizzazione consentendo di importare solo ciò di cui si ha bisogno:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Esempio di output: 2023-04-14
```

Ogni approccio offre diversi livelli di comodità e flessibilità per lavorare con le date in JavaScript, dall'oggetto `Date` incorporato fino alle capacità di formattazione e manipolazione più sofisticate disponibili tramite librerie.
