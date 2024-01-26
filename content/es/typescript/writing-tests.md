---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?

Escribir pruebas es crear verificaciones automáticas para tu código. Los programadores lo hacen para asegurarse de que sus programas funcionen correctamente, detectar fallos rápido y mejorar la calidad del software.

## Cómo Hacerlo:

Escribe una prueba básica con Jest, una librería popular para pruebas en TypeScript:

```typescript
import sumar from './sumar';

test('suma 1 + 2 para obtener 3', () => {
  expect(sumar(1, 2)).toBe(3);
});
```

Archivo `sumar.ts`:

```typescript
function sumar(a: number, b: number): number {
  return a + b;
}

export default sumar;
```

Corre las pruebas con `npm test` y obtén algo así:

```
PASS  ./sumar.test.ts
✓ suma 1 + 2 para obtener 3 (5ms)
```

## Profundización

Las pruebas automatizadas comenzaron en los años 50. Jest es solo una opción. Alternativas incluyen Mocha, Jasmine y Tape. Para TypeScript, es esencial usar tipos para capturar errores en las pruebas. Importa tipos de Jest así:

```typescript
import '@types/jest';
```

Esto ayuda a que el editor de código ofrezca sugerencias y detección de errores en tiempo real.

## Ver También:

- Documentación de Jest: [jestjs.io](https://jestjs.io/)
