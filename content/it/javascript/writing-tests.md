---
title:                "Scrivere test"
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere test nel codice significa creare piccoli programmi che verificano se le parti del tuo software funzionano come previsto. I programmatori scrivono test per assicurarsi che il codice sia affidabile e per prevenire regressioni durante l'aggiunta di nuove funzionalità.

## Come Fare:
Ecco un esempio semplice usando Jest, un framework di test JavaScript:

```Javascript
// somma.js
function somma(a, b) {
    return a + b;
}

module.exports = somma;
```

```Javascript
// somma.test.js
const somma = require('./somma');

test('somma 1 + 2 to equal 3', () => {
    expect(somma(1, 2)).toBe(3);
});
```

Esegui il test con:

```bash
$ npm run test
```

Output atteso:

```bash
PASS  ./somma.test.js
✓ somma 1 + 2 to equal 3 (5ms)
```

## Approfondimento:
La pratica di scrivere test si sviluppò negli anni '90 con l'affermarsi dell'Extreme Programming (XP), ponendo le basi per metodi come il Test-Driven Development (TDD). Alternative a Jest includono Mocha, Jasmine e QUnit. Quando scrivi test, considera aspetti come il mocking delle dipendenze, il controllo dell'ambiente di test e la copertura del codice.

## Vedi Anche:
- Documentazione di Jest: https://jestjs.io/it/
- Guida al Test-Driven Development: https://www.agilealliance.org/glossary/tdd/
- Confronto tra framework di test JavaScript: https://medium.com/welldone-software/an-overview-of-javascript-testing-7ce7298b9870
