---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:04.430123-07:00
description: 'Wie geht das: #.'
lastmod: '2024-03-13T22:44:54.269630-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Tests Schreiben
weight: 36
---

## Wie geht das:


### Native Ansatz (mit Jest)
Jest ist ein beliebtes Test-Framework, das eine benutzerfreundliche API für das Schreiben von Unit-Tests in JavaScript bietet. Es erfordert minimale Konfiguration und kommt mit Funktionen wie Mock-Funktionen, Timern und Snapshot-Tests.

1. **Installation**:

```bash
npm install --save-dev jest
```

2. **Einen einfachen Test schreiben**:

Erstelle eine Datei namens `sum.test.js`:

```javascript
const sum = require('./sum'); // Nehmen wir an, diese Funktion addiert einfach zwei Zahlen

test('addiert 1 + 2 zu 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **Deinen Test ausführen**:

```bash
npx jest
```

**Beispielausgabe:**

```plaintext
PASS  ./sum.test.js
✓ addiert 1 + 2 zu 3 (5ms)
```

### Asynchronen Code testen
Jest erleichtert das Testen von Promises und asynchroner/await-Syntax:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('asynchrone Addition funktioniert', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### Verwendung von Drittanbieter-Bibliotheken (Mocha & Chai)
Mocha ist ein weiteres beliebtes Test-Framework, das oft mit der Assertion-Bibliothek Chai für aussagekräftigere Tests verwendet wird.

1. **Installation**:

```bash
npm install --save-dev mocha chai
```

2. **Einen Test mit Mocha und Chai schreiben**:

Erstelle `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // Ein einfaches Berechnungsmodul

describe('Calculate', function() {
  it('sollte zwei Werte summieren', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Deine Tests mit Mocha ausführen**:

Füge ein Skript in deine `package.json` hinzu:

```json
"scripts": {
  "test": "mocha"
}
```

Dann führe aus:

```bash
npm test
```

**Beispielausgabe:**

```plaintext
  Calculate
    ✓ sollte zwei Werte summieren


  1 passing (8ms)
```

Diese Beispiele illustrieren die Grundlagen des Test-Schreibens und der Ausführung in JavaScript. Die Einführung eines Test-Frameworks wie Jest oder Mocha mit Chai kann eine solide Grundlage für robustes Anwendungstesting bieten und helfen sicherzustellen, dass Ihr Code wie beabsichtigt funktioniert, über Updates und Refactorings hinweg.
