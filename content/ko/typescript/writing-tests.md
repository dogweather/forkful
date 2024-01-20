---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
테스트 작성은 코드가 예상대로 작동하는지 검증하는 과정입니다. 버그를 예방하고, 리팩토링에 확신을 주며, 소프트웨어 품질을 유지하기 위해 합니다.

## How to:
```TypeScript
// sum.ts 파일
export function sum(a: number, b: number): number {
    return a + b;
}

// sum.test.ts 파일
import { sum } from './sum';

describe('sum 함수 테스트', () => {
    test('1 + 2가 3이어야 함', () => {
        expect(sum(1, 2)).toBe(3);
    });
});
```
코드 실행 결과:
```
PASS  ./sum.test.ts
✓ 1 + 2가 3이어야 함 (5ms)
```

## Deep Dive
테스트는 1950년대부터 있었습니다. 대안으로 TDD(Test-Driven Development), BDD(Behavior-Driven Development) 등이 있어요. 구현 세부사항에는 Jest, Mocha 같은 테스팅 프레임워크를 사용하며, 종류에는 유닛 테스트, 통합 테스트, E2E 테스트 등이 있습니다.

## See Also
- Mocha 공식 사이트: [https://mochajs.org/](https://mochajs.org/)