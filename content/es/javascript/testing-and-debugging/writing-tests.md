---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:03.015066-07:00
description: "Escribir pruebas en JavaScript se refiere a la pr\xE1ctica de crear\
  \ scripts automatizados que ejecutan tu c\xF3digo para asegurarse de que se comporte\
  \ como se\u2026"
lastmod: '2024-02-25T18:49:55.931621-07:00'
model: gpt-4-0125-preview
summary: "Escribir pruebas en JavaScript se refiere a la pr\xE1ctica de crear scripts\
  \ automatizados que ejecutan tu c\xF3digo para asegurarse de que se comporte como\
  \ se\u2026"
title: Escribiendo pruebas
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir pruebas en JavaScript se refiere a la práctica de crear scripts automatizados que ejecutan tu código para asegurarse de que se comporte como se espera, lo que puede mejorar significativamente la fiabilidad y mantenibilidad de tus aplicaciones. Los programadores hacen esto para detectar errores temprano, facilitar la refactorización del código y asegurar que las nuevas características no rompan la funcionalidad existente.

## Cómo hacerlo:

### Enfoque Nativo (usando Jest)

Jest es un marco de prueba popular que proporciona una API amigable para escribir pruebas unitarias en JavaScript. Requiere una configuración mínima y viene con características como funciones simuladas, temporizadores y pruebas de instantáneas.

1. **Instalación**:

```bash
npm install --save-dev jest
```

2. **Escribir una prueba simple**:

Crea un archivo llamado `sum.test.js`:

```javascript
const sum = require('./sum'); // Supongamos que esta función simplemente suma dos números

test('suma 1 + 2 para igualar 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **Ejecutar tu prueba**:

```bash
npx jest
```

**Salida de Ejemplo:**

```plaintext
PASS  ./sum.test.js
✓ suma 1 + 2 para igualar 3 (5ms)
```

### Probando Código Asíncrono

Jest facilita la prueba de promesas y la sintaxis async/await:

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('la suma asíncrona funciona', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### Usando Bibliotecas de Terceros (Mocha & Chai)

Mocha es otro marco de prueba popular, a menudo utilizado con la biblioteca de aserciones Chai para pruebas más expresivas.

1. **Instalación**:

```bash
npm install --save-dev mocha chai
```

2. **Escribir una prueba con Mocha y Chai**:

Crea `calculate.test.js`:

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // Un módulo de cálculo simple

describe('Calculate', function() {
  it('debería sumar dos valores', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Ejecutar tus pruebas con Mocha**:

Agrega un script en tu `package.json`:

```json
"scripts": {
  "test": "mocha"
}
```

Luego ejecuta:

```bash
npm test
```

**Salida de Ejemplo:**

```plaintext
  Calculate
    ✓ debería sumar dos valores


  1 passing (8ms)
```

Estos ejemplos ilustran la escritura y ejecución básica de pruebas en JavaScript. Adoptar un marco de prueba como Jest o Mocha con Chai puede proporcionar una base sólida para pruebas robustas de aplicaciones, ayudando a asegurar que tu código funcione según lo previsto a través de actualizaciones y refactorizaciones.
