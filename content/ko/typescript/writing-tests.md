---
title:                "TypeScript: 테스트 작성"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

*왜* 누군가가 테스트를 작성하는 것에 관여하는 이유는 무엇일까요? 프로그래밍은 복잡한 작업이며, 우리는 코드가 예상한 대로 실행되고, 문제가 발생하면 해당 문제를 신속하게 파악하여 해결할 수 있어야 합니다. 이를 위해 테스트를 작성하는 것이 중요한데, 이를 통해 코드의 동작을 검증하고 예상치 못한 결과가 발생하는 것을 방지할 수 있습니다.


## 방법

우리는 TypeScript로 작성하는 테스트를 이해하기 쉽고 간단하게 작성할 수 있습니다. 먼저, 우리는 테스트를 위해 Jest라는 테스트 프레임워크를 사용할 것입니다. 다음은 TypeScript로 작성된 간단한 함수의 테스트 예제입니다.

```TypeScript
// sum.js 파일
export const sum = (a, b) => {
  return a + b;
}

// sum.test.js 파일
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

위의 예제에서 우리는 먼저 `sum.js` 파일에 `sum` 함수를 정의했습니다. 그리고 `sum.test.js` 파일에서는 `sum` 함수를 가져와서 1과 2를 더한 결과가 3이 나오는지를 테스트하고 있습니다. 우리는 `expect()`와 `toBe()` 함수를 사용하여 실제 결과와 예상한 결과가 같은지를 확인할 수 있습니다. 이렇게 간단한 예제에서도 테스트를 작성하고 실행할 수 있습니다.


## 딥 다이브

테스트는 우리의 코드를 검증하고 버그를 최소화하는데 중요한 역할을 합니다. 테스트를 작성함으로써 우리는 코드의 예상치 못한 부분을 발견하고 해당 문제를 해결할 수 있습니다. 또한 테스트를 작성하는 것은 우리의 코드를 좀 더 깨끗하고 잘 구조화할 수 있도록 도와줍니다. Jest는 유연하고 다양한 기능들을 제공하여 보다 강력한 테스트를 작성할 수 있도록 도와줍니다. 또한 TypeScript와 함께 사용하면 더욱 간편하게 테스트를 작성할 수 있습니다.

더 자세한 정보는 공식 Jest 문서를 참고하거나 TypeScript 공식 문서에서 테스트 작성 방법을 참고할 수 있습니다.


## 참고

* 다양한 기능을 제공하는 Jest 테스트 프레임워크: https://jestjs.io/
* TypeScript에서의 테스트 작성 방법: https://www.typescriptlang.org/docs/handbook/testing.html