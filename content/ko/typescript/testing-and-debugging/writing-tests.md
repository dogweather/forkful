---
title:                "테스트 작성하기"
aliases:
- /ko/typescript/writing-tests/
date:                  2024-02-03T19:32:33.502674-07:00
model:                 gpt-4-0125-preview
simple_title:         "테스트 작성하기"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
TypeScript에서 테스트를 작성한다는 것은 코드의 기능성과 정확성을 검증하기 위한 자동화된 스크립트를 만드는 것을 의미합니다. 프로그래머들은 신뢰성을 보장하고, 버그를 빠르게 발견하며, 유지보수 가능한 코드 성장을 용이하게 하기 위해 이 작업을 합니다. TypeScript의 정적 타이핑은 JavaScript 테스트에 예측 가능성을 추가해 줍니다.

## 방법:
TypeScript는 대부분의 JavaScript 테스트 프레임워크와 조화롭게 작동합니다. 시연 목적으로 TypeScript 프로젝트의 설정이 필요 없는 인기 있는 테스트 프레임워크인 Jest를 사용할 것입니다.

먼저, Jest와 필요한 TypeScript 타입이 설치되어 있는지 확인하세요:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

다음으로, `jest.config.js`를 수정하거나 새로 만들어 TypeScript에서 Jest를 사용하도록 설정하세요:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

이제, 간단한 함수와 그것에 대한 테스트를 작성해 봅시다. 다음 함수를 포함한 `sum.ts` 파일을 고려해 보세요:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

`sum.test.ts`라는 테스트 파일을 생성하세요:

```typescript
// sum.test.ts
import { sum } from './sum';

test('1 + 2를 더해서 3이 되는지', () => {
  expect(sum(1, 2)).toBe(3);
});
```

테스트를 실행하려면 다음을 사용하세요:

```bash
npx jest
```

테스트가 통과되었음을 나타내는 샘플 출력은 다음과 같아야 합니다:

```plaintext
 PASS  ./sum.test.ts
  ✓ 1 + 2를 더해서 3이 되는지 (2 ms)
```

비동기 코드의 경우, Jest는 `async/await`를 사용하여 수용합니다. 비동기 `fetchData` 함수가 있다고 가정합시다:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

비동기 함수를 사용한 테스트:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('데이터를 성공적으로 가져옴', async () => {
  expect(await fetchData()).toBe('data');
});
```

테스트를 실행할 때, Jest는 프로미스가 해결될 때까지 기다릴 것이며, 비동기 작업을 올바르게 테스트할 것입니다.

효과적인 테스트는 여러 시나리오를 포함한 다수의 테스트를 작성하는 것을 포함하며, 이는 타입스크립트 코드가 예상대로 동작함을 보장하기 위한 경계 조건을 포함합니다.
