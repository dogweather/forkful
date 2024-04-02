---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:20.514203-07:00
description: "Pisanie test\xF3w w TypeScript polega na tworzeniu automatycznych skrypt\xF3\
  w sprawdzaj\u0105cych funkcjonalno\u015B\u0107 i poprawno\u015B\u0107 Twojego kodu.\
  \ Programi\u015Bci robi\u0105 to, aby\u2026"
lastmod: '2024-03-13T22:44:35.142439-06:00'
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w TypeScript polega na tworzeniu automatycznych skrypt\xF3\
  w sprawdzaj\u0105cych funkcjonalno\u015B\u0107 i poprawno\u015B\u0107 Twojego kodu.\
  \ Programi\u015Bci robi\u0105 to, aby\u2026"
title: "Pisanie test\xF3w"
weight: 36
---

## Co i Dlaczego?
Pisanie testów w TypeScript polega na tworzeniu automatycznych skryptów sprawdzających funkcjonalność i poprawność Twojego kodu. Programiści robią to, aby zapewnić niezawodność, szybko łapać błędy i ułatwić utrzymywanie wzrostu kodu, ponieważ statyczne typowanie TypeScript dodaje poziom przewidywalności do testowania JavaScript.

## Jak to zrobić:
TypeScript doskonale współpracuje z większością frameworków do testowania JavaScript. Dla celów demonstracyjnych użyjemy Jest, popularnego frameworka testowego, ze względu na jego konfigurację zerową dla projektów TypeScript.

Najpierw upewnij się, że masz zainstalowany Jest oraz niezbędne typy TypeScript:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

Następnie, skonfiguruj Jest do pracy z TypeScript poprzez modyfikację pliku `jest.config.js` lub jeśli tworzysz nowy:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Teraz, napiszmy prostą funkcję i test do niej. Rozważ plik `sum.ts` z następującą funkcją:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

Stwórz plik testowy o nazwie `sum.test.ts`:

```typescript
// sum.test.ts
import { sum } from './sum';

test('dodaje 1 + 2, aby uzyskać 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

Uruchom swoje testy za pomocą:

```bash
npx jest
```

Przykładowe wyniki wskazujące na zaliczony test powinny wyglądać mniej więcej tak:

```plaintext
 PASS  ./sum.test.ts
  ✓ dodaje 1 + 2, aby uzyskać 3 (2 ms)
```

Dla kodu asynchronicznego, Jest obsługuje `async/await`. Załóżmy, że masz asynchroniczną funkcję `fetchData`:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

Twój test używający funkcji asynchronicznych:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('pobiera dane z powodzeniem', async () => {
  expect(await fetchData()).toBe('data');
});
```

Uruchamiając swoje testy, Jest będzie czekał na rozwiązanie obietnicy, poprawnie testując operacje asynchroniczne.

Pamiętaj, skuteczne testowanie obejmuje pisanie wielu testów dla różnych scenariuszy, w tym przypadków brzegowych, aby upewnić się, że Twój kod TypeScript działa zgodnie z oczekiwaniami.
