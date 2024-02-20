---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:02.773573-07:00
description: "Scrivere test in JavaScript si riferisce alla pratica di creare script\
  \ automatizzati che eseguono il tuo codice per assicurarsi che si comporti come\u2026"
lastmod: 2024-02-19 22:05:02.889742
model: gpt-4-0125-preview
summary: "Scrivere test in JavaScript si riferisce alla pratica di creare script automatizzati\
  \ che eseguono il tuo codice per assicurarsi che si comporti come\u2026"
title: Scrivere test
---

{{< edit_this_page >}}

## Cosa e Perché?

Scrivere test in JavaScript si riferisce alla pratica di creare script automatizzati che eseguono il tuo codice per assicurarsi che si comporti come previsto, il che può migliorare significativamente l'affidabilità e la manutenibilità delle tue applicazioni. I programmatori fanno ciò per individuare precocemente i bug, facilitare il refactoring del codice e garantire che le nuove funzionalità non compromettano la funzionalità esistente.

## Come fare:

### Approccio Nativo (usando Jest)

Jest è un framework di test popolare che fornisce un'API amichevole per scrivere test unitari in JavaScript. Richiede una configurazione minima e viene fornito con funzionalità come funzioni mock, timer e test degli snapshot.

1. **Installazione**:

```bash
npm install --save-dev jest
```

2. **Scrivere un test semplice**:

Crea un file chiamato `sum.test.js`:

```javascript
const sum = require('./sum'); // Assume che questa funzione sommi semplicemente due numeri

test('aggiunge 1 + 2 per ottenere 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **Eseguire il tuo test**:

```bash
npx jest
```

**Output di Esempio:**

```plaintext
PASS  ./sum.test.js
✓ aggiunge 1 + 2 per ottenere 3 (5ms)
```

### Testare Codice Asincrono

Jest semplifica il test di promesse e della sintassi async/await:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// test di asyncSum.js
test('l'addizione asincrona funziona', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### Utilizzare Librerie di Terze Parti (Mocha & Chai)

Mocha è un altro framework di test popolare, spesso utilizzato con la libreria di asserzioni Chai per test più espressivi.

1. **Installazione**:

```bash
npm install --save-dev mocha chai
```

2. **Scrivere un test con Mocha e Chai**:

Crea `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // Un modulo di calcolo semplice

describe('Calculate', function() {
  it('dovrebbe sommare due valori', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Eseguire i tuoi test con Mocha**:

Aggiungi uno script nel tuo `package.json`:

```json
"scripts": {
  "test": "mocha"
}
```

Poi esegui:

```bash
npm test
```

**Output di Esempio:**

```plaintext
  Calculate
    ✓ dovrebbe sommare due valori


  1 passing (8ms)
```

Questi esempi illustrano la scrittura e l'esecuzione di base dei test in JavaScript. Adottare un framework di test come Jest o Mocha con Chai può fornire una base solida per robusti test delle applicazioni, contribuendo a garantire che il tuo codice funzioni come previsto attraverso aggiornamenti e rifacimenti.
