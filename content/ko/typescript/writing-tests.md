---
title:                "TypeScript: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

코드의 안전성과 신뢰성을 보장하기 위해서, 테스트 코드를 작성하는 것이 중요합니다. 이는 버그와 결함을 발견하여 제거하고, 코드의 동작을 확실히 보장하여 소프트웨어의 품질을 높이기 위함입니다.

## 방법

테스트는 대부분 개발 과정에서 빠지지 않고 수행되어야 합니다. 이를 위해 TypeScript에서는 `jest`를 사용하여 자동화된 테스트를 할 수 있습니다. 아래는 `jest`를 사용하여 간단히 `Calculator` 클래스를 테스트하는 예시입니다.

```TypeScript
// Calculator.ts
class Calculator {
  add(a: number, b: number): number {
    return a + b;
  }
}

// Calculator.test.ts
import Calculator from './Calculator';

test('add two numbers', () => {
  const calculator = new Calculator();
  expect(calculator.add(2, 3)).toBe(5);
});
```

테스트 코드는 `Calculator` 클래스의 `add()` 메소드를 호출하여 2와 3을 더한 결과가 5인지를 검증합니다. 이를 위해 `expect()`와 `toBe()` 메소드를 사용합니다.

## 딥 다이브

테스트 코드 작성에는 여러 가지 방법과 규칙이 있지만, 중요한 것은 코드를 어떻게 테스트할지에 대한 고민을 하는 것입니다. 효율적인 테스트 작성을 위해서는 각 함수, 메소드 또는 모듈을 작은 단위로 분할하여 각각을 독립적으로 테스트하는 것이 중요합니다.

또한 `jest`를 비롯한 다양한 테스트 라이브러리에서는 다양한 기능을 제공하여 테스트를 보다 쉽고 편리하게 작성할 수 있도록 도와줍니다. 이러한 기능을 잘 활용하면 테스트 코드의 가독성을 높이고, 유지보수를 편리하게 할 수 있습니다.

## 참고

- [jest 공식 문서](https://jestjs.io/docs/getting-started)
- [타입스크립트 공식 문서 - 테스팅](https://www.typescriptlang.org/docs/handbook/testing.html)
- [타입스크립트와 jest를 이용한 유닛 테스트 작성하기](https://velog.io/@velopert/typescript-and-jest)