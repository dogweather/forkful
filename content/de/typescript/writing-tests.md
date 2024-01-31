---
title:                "Tests schreiben"
date:                  2024-01-19
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"

category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben von Tests bedeutet, Code-Stücke zu erstellen, die anderen Code überprüfen. Programmierer tun das, um Fehler schnell zu finden und die Softwarequalität sicherzustellen.

## How to:
Ein Beispiel für einen simplen Unit-Test mit Jest:

```TypeScript
import { sum } from './sum';

test('summiert 1 + 2 gleich 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Ausgabe-Beispiel wenn der Test erfolgreich ist:

```
PASS  ./sum.test.ts
✓ summiert 1 + 2 gleich 3 (5ms)
```

Für End-to-End-Tests mit Playwright:

```TypeScript
import { chromium } from 'playwright';

(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage();
  await page.goto('http://localhost:3000/');
  await expect(page).toHaveText('Hello world');
  await browser.close();
})();
```

## Deep Dive
Tests gibt es, seit Software entwickelt wird. Frameworks wie Mocha, Jasmine und Jest bei JavaScript/TypeScript haben das Testen vereinfacht. Bei End-to-End-Tests wird oft Selenium genutzt, aber Playwright ist populär für moderne Web-Apps. Im Detail: Tests isolieren Funktionalitäten und prüfen sie individuell (Unit-Tests) oder als Teil des Gesamtsystems (Integrationstests, End-to-End-Tests).

## See Also
- Jest: [https://jestjs.io](https://jestjs.io)
- Playwright: [https://playwright.dev](https://playwright.dev)
- Mocha: [https://mochajs.org](https://mochajs.org)
- Jasmine: [https://jasmine.github.io](https://jasmine.github.io)
