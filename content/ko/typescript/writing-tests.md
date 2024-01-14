---
title:    "TypeScript: 컴퓨터 프로그래밍에서의 테스트 작성"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

코드 테스트는 개발 과정에서 매우 중요한 요소입니다. 이를 통해 코드의 신뢰성을 높일 수 있고 버그 발생 가능성을 최소화할 수 있습니다. 따라서 프로그래머들은 효율적이고 안정적인 코드를 작성하기 위해 테스트를 작성해야 합니다.

## 어떻게

자바스크립트의 강력한 형식 검사 기능 중 하나인 TypeScript를 사용하여 간단한 코드 테스트를 작성해보겠습니다. 우선, 테스트 파일을 만들고 `describe` 함수를 사용하여 테스트의 범위를 정의합니다. 그리고 `it` 함수를 사용하여 각각의 테스트 케이스를 작성합니다. 아래는 간단한 계산기 함수에 대한 테스트 예시입니다.
```TypeScript
// 테스트 파일
describe('계산기 함수', () => {
  it('두 수를 더할 수 있어야 함', () => {
    expect(add(3, 5)).toBe(8);
    expect(add(-2, 7)).toBe(5);
  });

  it('두 수를 뺄 수 있어야 함', () => {
    expect(subtract(9, 3)).toBe(6);
    expect(subtract(4, -2)).toBe(6);
  });

  it('두 수를 곱할 수 있어야 함', () => {
    expect(multiply(4, 3)).toBe(12);
    expect(multiply(-5, 2)).toBe(-10);
  });

  it('두 수를 나눌 수 있어야 함', () => {
    expect(divide(10, 2)).toBe(5);
    expect(divide(8, -4)).toBe(-2);
  });
});
```
위 코드에서는 `expect` 함수를 사용하여 예상 결과값을 지정하고, `toBe` 함수를 사용하여 실제 결과값과 비교합니다. 이렇게 작성된 테스트는 `npm test` 명령을 통해 실행할 수 있습니다. 만약 덧셈 함수가 잘 작동하지 않는다면, 해당 테스트는 실패할 것입니다. 이렇게 테스트 결과를 통해 코드의 문제를 파악하고 수정할 수 있습니다.

## 딥 다이브

이 예시에서는 `expect`와 `toBe` 함수를 사용했지만, TypeScript에서는 다양한 검증 함수를 지원합니다. 예를 들어 `toBeCloseTo`, `toEqual`, `toContain` 등 다양한 함수를 이용하여 테스트를 작성할 수 있습니다. 또한 `beforeEach`나 `afterEach` 등의 함수를 사용하여 테스트 실행 전/후에 필요한 작업을 수행할 수도 있습니다. 이런 다양한 기능들을 알고 있으면 보다 복잡한 테스트를 작성할 수 있고, 코드의 신뢰성을 보다 높일 수 있습니다.

## 참고 자료

- [TypeScript 공식 홈페이지](https://typescriptlang.org/)
- [Jasmine Testing Library 사용법](https://jasmine.github.io/pages/getting_started.html)
- [Effective TypeScript: 가독성 좋은 TypeScript 코드 작성을 위한 팁](https://effective-typescript.job4u.kr/)