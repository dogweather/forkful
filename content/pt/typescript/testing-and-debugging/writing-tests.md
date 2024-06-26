---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:08.534062-07:00
description: "Como fazer: O TypeScript funciona em harmonia com a maioria dos frameworks\
  \ de teste em JavaScript. Para fins de demonstra\xE7\xE3o, usaremos o Jest, um\u2026"
lastmod: '2024-03-13T22:44:46.329677-06:00'
model: gpt-4-0125-preview
summary: O TypeScript funciona em harmonia com a maioria dos frameworks de teste em
  JavaScript.
title: Escrevendo testes
weight: 36
---

## Como fazer:
O TypeScript funciona em harmonia com a maioria dos frameworks de teste em JavaScript. Para fins de demonstração, usaremos o Jest, um framework de teste popular, devido à sua configuração zero para projetos TypeScript.

Primeiro, certifique-se de que você tenha o Jest e os tipos necessários do TypeScript instalados:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Em seguida, configure o Jest para trabalhar com TypeScript modificando o `jest.config.js` ou criando um novo:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Agora, vamos escrever uma função simples e um teste para ela. Considere um arquivo `sum.ts` com a seguinte função:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Crie um arquivo de teste chamado `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('soma 1 + 2 para igual a 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Execute seus testes com:

```bash
npx jest
```

Um exemplo de saída indicando um teste aprovado deve parecer algo assim:

```plaintext
 PASS  ./sum.test.ts
  ✓ soma 1 + 2 para igual a 3 (2 ms)
```

Para código assíncrono, o Jest acomoda com `async/await`. Suponha que você tenha uma função assíncrona `fetchData`:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Seu teste usando funções assíncronas:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('busca dados com sucesso', async () => {
  expect(await fetchData()).toBe('data');
});
```

Ao executar seus testes, o Jest esperará a promessa ser resolvida, testando corretamente as operações assíncronas.

Lembre-se, testes eficazes incluem escrever múltiplos testes para diferentes cenários, incluindo casos extremos, para garantir que o seu código TypeScript se comporte como esperado.
