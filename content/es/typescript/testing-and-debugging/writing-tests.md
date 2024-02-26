---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:40.782813-07:00
description: "Escribir pruebas en TypeScript implica crear scripts automatizados para\
  \ verificar la funcionalidad y la correcci\xF3n de tu c\xF3digo. Los programadores\
  \ lo\u2026"
lastmod: '2024-02-25T18:49:55.303196-07:00'
model: gpt-4-0125-preview
summary: "Escribir pruebas en TypeScript implica crear scripts automatizados para\
  \ verificar la funcionalidad y la correcci\xF3n de tu c\xF3digo. Los programadores\
  \ lo\u2026"
title: Escribiendo pruebas
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir pruebas en TypeScript implica crear scripts automatizados para verificar la funcionalidad y la corrección de tu código. Los programadores lo hacen para garantizar la fiabilidad, capturar rápidamente errores y facilitar el crecimiento mantenible del código, ya que la tipificación estática de TypeScript añade un nivel de previsibilidad a las pruebas de JavaScript.

## Cómo hacerlo:
TypeScript funciona armoniosamente con la mayoría de los marcos de pruebas de JavaScript. Para fines de demostración, utilizaremos Jest, un marco de pruebas popular, debido a su configuración cero para proyectos TypeScript.

Primero, asegúrate de tener Jest y los tipos de TypeScript necesarios instalados:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Luego, configura Jest para trabajar con TypeScript modificando el `jest.config.js` o creando uno nuevo:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Ahora, escribamos una función simple y una prueba para ella. Considera un archivo `sum.ts` con la siguiente función:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Crea un archivo de prueba llamado `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('suma 1 + 2 para igual a 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Ejecuta tus pruebas con:

```bash
npx jest
```

La salida de muestra que indica una prueba superada debería verse algo así:

```plaintext
 PASS  ./sum.test.ts
  ✓ suma 1 + 2 para igual a 3 (2 ms)
```

Para código asincrónico, Jest se acomoda con `async/await`. Supongamos que tienes una función asincrónica `fetchData`:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Tu prueba usando funciones asíncronas:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('obtiene datos exitosamente', async () => {
  expect(await fetchData()).toBe('data');
});
```

Cuando ejecutas tus pruebas, Jest esperará a que la promesa se resuelva, probando correctamente las operaciones asincrónicas.

Recuerda, la prueba efectiva incluye escribir múltiples pruebas para diferentes escenarios, incluidos casos límite, para asegurar que tu código TypeScript se comporte como se espera.
