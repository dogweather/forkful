---
title:                "Skriving av tester"
date:                  2024-01-19
html_title:           "Arduino: Skriving av tester"
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testskriving er prosessen med å lage skript som automatisk sjekker at koden gjør det den skal. Vi gjør dette for å unngå bugs, sikre kvalitet, og spare tid på manuell testing.

## How to:
I JavaScript kan vi bruke Jest-rammeverket for å skrive tester. Her er et enkelt eksempel:

```javascript
// sum.js
function sum(a, b) {
  return a + b;
}
module.exports = sum;

// sum.test.js
const sum = require('./sum');

test('adderer 1 + 2 for å gi 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```
Kjør testene med `npm test`. Forventet output:

```
PASS  ./sum.test.js
✓ adderer 1 + 2 for å gi 3 (5ms)
```

## Deep Dive
Tester i JavaScript har rot i TDD (Test-Driven Development) som begynte å ta form på 2000-tallet. Alternativer til Jest inkluderer Mocha, Jasmine og QUnit. Tester kan innebære rene funksjonstester, integrasjonstester, eller end-til-end tester (E2E). En god testingpraksis handler om å finne en balanse mellom testtyper basert på prosjektets behov.

## See Also
- Jest dokumentasjon: [Jestjs.io](https://jestjs.io/)
- Andre testrammeverk: [Mocha](https://mochajs.org/), [Jasmine](https://jasmine.github.io/), [QUnit](https://qunitjs.com/)
