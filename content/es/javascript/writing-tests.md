---
title:                "Escribiendo pruebas"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir tests significa crear pruebas automáticas que verifican si los diferentes partes de tu código funcionan como esperas. Los programadores escriben tests para ahorrar tiempo, evitar errores futuros y garantizar que los cambios no rompan funcionalidades existentes.

## Cómo Hacerlo:
Para escribir tests en JavaScript, puedes utilizar librerías como Jest. Aquí tienes un ejemplo sencillo de cómo se vería un test para una función que suma dos números:

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

test('suma 1 + 2 para dar como resultado 3', () => {
    expect(sum(1, 2)).toBe(3);
});
```

Línea de comando para correr Jest:
```bash
$ npm run test
```

Resultado esperado:
```
PASS ./sum.test.js
✓ suma 1 + 2 para dar como resultado 3 (5ms)
```

## Análisis Profundo:
Historialmente, se ha hecho testing manualmente, pero esto resulta ser ineficiente y propenso a omitir errores. Ahora, la automatización es la clave. Alternativas a Jest incluyen Mocha, Jasmine y QUnit, entre otros. Vale la pena explorar la documentación oficial de Jest para aprender sobre mocks, spies y el ciclo de vida de los tests. Asegúrate de entender conceptos de testing como "unit tests", "integration tests" y "end-to-end tests" y cuándo aplicar cada uno.

## Ver También:
Puedes ampliar tu conocimiento sobre testing en JavaScript con las siguientes fuentes:

- Documentación oficial de Jest: [jestjs.io](https://jestjs.io/)
- Comparación de frameworks de testing en JavaScript: [https://stateofjs.com/](https://stateofjs.com/)
- Artículos y guías sobre testing de JavaScript en [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Learn/Tools_and_testing/Client-side_JavaScript_frameworks/Introduction)
- Series de videos y tutoriales didácticos en [YouTube](https://www.youtube.com/results?search_query=javascript+testing)
