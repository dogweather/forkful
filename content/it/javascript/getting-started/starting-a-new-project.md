---
date: 2024-01-20 18:03:59.259362-07:00
description: "Partire con un nuovo progetto JavaScript significa gettare le basi per\
  \ un\u2019applicazione. I programmatori lo fanno per trasformare idee in realt\xE0\
  \ digitale,\u2026"
lastmod: '2024-03-11T00:14:17.430960-06:00'
model: gpt-4-1106-preview
summary: "Partire con un nuovo progetto JavaScript significa gettare le basi per un\u2019\
  applicazione. I programmatori lo fanno per trasformare idee in realt\xE0 digitale,\u2026"
title: Avvio di un nuovo progetto
---

{{< edit_this_page >}}

## What & Why?
Partire con un nuovo progetto JavaScript significa gettare le basi per un’applicazione. I programmatori lo fanno per trasformare idee in realtà digitale, cominciando da un ambiente pulito.

## How to:
Creiamo un progetto base usando Node.js che funge da ambiente di runtime per JavaScript.

```Javascript
// 1. Inizializza un nuovo progetto Node.js
// Apri il terminale e digita:
npm init -y

// 2. Crea un file 'app.js'
// Usa il tuo editor di testo preferito per scrivere:
console.log('Ciao, mondo!');

// 3. Esegui l'applicazione
// Torna al terminale e digita:
node app.js
```
Output:
```
Ciao, mondo!
```

## Deep Dive
Iniziare un progetto JavaScript era molto diverso nel passato. Prima dei moderni strumenti come NPM e Node.js, ci si affidava a semplici file HTML con script inline o collegamenti a file JavaScript esterni. Ora, gestiamo dipendenze complesse, ambiente di sviluppo (come Webpack o Babel), e frameworks come React o Vue. L'ecosistema moderno migliora organizzazione e standardizzazione, ma introduce complessità.

Alternatives al `npm init` includono l’uso di boilerplates o generatori di progetto come `create-react-app` o `vue-cli` che configurano automaticamente molte opzioni per te.

Per quanto riguarda l'implementazione, inizia con un `package.json` pulito e man mano aggiungi scripts e dipendenze. Ricorda di mantenere il tuo progetto aggiornato e di usare un `.gitignore` per escludere file non necessari dal controllo versione.

## See Also
- Documentazione ufficiale di Node.js: https://nodejs.org/
- NPM scripts e package.json: https://docs.npmjs.com/cli/v7/using-npm/scripts
- `create-react-app`: https://reactjs.org/docs/create-a-new-react-app.html
- `vue-cli`: https://cli.vuejs.org/guide/
- `.gitignore` per Node.js: https://github.com/github/gitignore/blob/master/Node.gitignore
