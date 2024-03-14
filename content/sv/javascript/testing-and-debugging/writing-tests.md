---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:17.969039-07:00
description: "Att skriva tester i JavaScript avser praktiken att skapa automatiserade\
  \ skript som k\xF6r din kod f\xF6r att s\xE4kerst\xE4lla att den fungerar som f\xF6\
  rv\xE4ntat, vilket\u2026"
lastmod: '2024-03-13T22:44:38.296931-06:00'
model: gpt-4-0125-preview
summary: "Att skriva tester i JavaScript avser praktiken att skapa automatiserade\
  \ skript som k\xF6r din kod f\xF6r att s\xE4kerst\xE4lla att den fungerar som f\xF6\
  rv\xE4ntat, vilket\u2026"
title: Skriva tester
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i JavaScript avser praktiken att skapa automatiserade skript som kör din kod för att säkerställa att den fungerar som förväntat, vilket kan förbättra tillförlitligheten och underhållet av dina applikationer avsevärt. Programmerare gör detta för att fånga buggar tidigt, underlätta kodrefaktorisering och säkerställa att nya funktioner inte bryter befintlig funktionalitet.

## Hur man gör:

### Nativt tillvägagångssätt (med Jest)

Jest är ett populärt testramverk som tillhandahåller ett användarvänligt API för att skriva enhetstester i JavaScript. Det kräver minimal konfiguration och kommer med funktioner som mockfunktioner, timers och snapshot-testning.

1. **Installation**:

```bash
npm install --save-dev jest
```

2. **Skriva ett enkelt test**:

Skapa en fil med namnet `sum.test.js`:

```javascript
const sum = require('./sum'); // Antag att denna funktion helt enkelt lägger till två nummer

test('lägger till 1 + 2 för att bli 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **Köra ditt test**:

```bash
npx jest
```

**Exempel på utskrift:**

```plaintext
PASS  ./sum.test.js
✓ lägger till 1 + 2 för att bli 3 (5ms)
```

### Testa Asynkron Kod

Jest gör det enkelt att testa löften och syntax för async/await:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('asynkron addition fungerar', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### Använda Tredjepartsbibliotek (Mocha & Chai)

Mocha är ett annat populärt testramverk, ofta använt med påståendebiblioteket Chai för mer uttrycksfulla tester.

1. **Installation**:

```bash
npm install --save-dev mocha chai
```

2. **Skriva ett test med Mocha och Chai**:

Skapa `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // En enkel beräkningsmodul

describe('Calculate', function() {
  it('bör summera två värden', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Köra dina tester med Mocha**:

Lägg till ett skript i din `package.json`:

```json
"scripts": {
  "test": "mocha"
}
```

Exekvera sedan:

```bash
npm test
```

**Exempel på utskrift:**

```plaintext
  Calculate
    ✓ bör summera två värden


  1 passing (8ms)
```

Dessa exempel illustrerar grundläggande skrivning och utförande av tester i JavaScript. Att anta ett testramverk som Jest eller Mocha med Chai kan ge en solid grund för robusta applikationstestningar, vilket hjälper till att säkerställa att din kod fungerar som avsett genom uppdateringar och refaktoriseringar.
