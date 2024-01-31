---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva tester är processen att koda för att automatiskt kontrollera att annan kod fungerar som den ska. Programmerare gör det för att säkerställa kvalitet, hitta fel tidigt och spara tid som annars skulle läggas på manuella tester.

## Hur gör man?:
Låt oss dyka in i ett grundläggande exempel med Jest, ett populärt testramverk för JavaScript.

```Javascript
// sum.js
function sum(a, b) {
  return a + b;
}
module.exports = sum;
```

```Javascript
// sum.test.js
const sum = require('./sum');

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

För att köra testet, använd följande kommando:

```Javascript
$ jest sum.test.js
```

Detta ska ge dig outputen:

```Javascript
PASS  ./sum.test.js
✓ adds 1 + 2 to equal 3 (5ms)
```

## Djupdykning:
Testningens historia går tillbaka till de första programmeringsdagarna, men verktygen har utvecklats avsevärt. Jämfört med äldre ramverk som JUnit (Java) är Jest snabbare och mer fokuserad på moderna JavaScript-appar. Alternativ till Jest inkluderar Mocha, Jasmine och Tape. När du skriver tester är det viktigt att täcka olika användningsfall, felhantering och gränsvärdeanalys, samtidigt som du håller testerna enkla och snabba.

## Se också:
- Jest's officiella hemsida: [https://jestjs.io/](https://jestjs.io/)
- JavaScript testning med Mocha: [https://mochajs.org/](https://mochajs.org/)
- Ytterligare läsning om testning i JavaScript: [MDN web docs](https://developer.mozilla.org/en-US/docs/Learn/Tools_and_testing/Client-side_JavaScript_frameworks/Introduction)
