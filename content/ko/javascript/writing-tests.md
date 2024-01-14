---
title:                "Javascript: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 테스트를 작성할까요?
코드를 작성하면서, 우리는 자주 코드가 제대로 동작하는지 확인하고 싶어합니다. 테스트를 작성하면, 우리는 코드의 여러 부분을 간단한 방법으로 검증할 수 있고, 코드의 품질을 보장할 수 있습니다.

## 작성하는 방법
우리는 `assert` 함수를 사용하여 테스트를 작성할 수 있습니다. 이 함수는 Boolean 값과 함께 비교하여 코드의 결과를 검증합니다. 예를 들어, 우리가 아래와 같은 코드를 가지고 있다고 합시다.

```Javascript
function add(a, b) {
  return a + b;
}

const result = add(2, 3);
```

우리는 `assert` 함수를 사용하여 이 함수가 제대로 동작하는지 확인할 수 있습니다.

```Javascript
assert(result === 5); // 테스트 통과
```

하지만 `assert` 함수만으로는 우리가 어떤 값이 잘못되었는지 알 수 없습니다. 이 때는 `expect` 함수를 사용하여 더 자세한 정보를 얻을 수 있습니다. 예를 들어,

```Javascript
expect(add(2, 3)).toBe(5); // 테스트 통과
expect(add(2, 2)).toBe(5); // 테스트 실패
```

`expect` 함수는 결과 값이 `toBe` 함수의 인자와 일치하는지를 확인합니다. 또한 `expect` 함수는 `not` 함수와 함께 사용하여 결과 값이 일치하지 않는지를 확인할 수도 있습니다.

```Javascript
expect(add(2, 2)).not.toBe(5); // 테스트 통과
```

마지막으로, 우리는 `describe` 함수를 사용하여 여러 테스트들을 그룹화할 수 있습니다. 예를 들어,

```Javascript
describe("add 함수", () => {
  it("두 수를 더합니다.", () => {
    expect(add(2, 2)).toBe(4); // 테스트 통과
  });
  
  it("두 개의 매개변수가 없으면 NaN을 반환합니다.", () => {
    expect(add()).toBe(NaN); // 테스트 통과
  });
});
```

## 깊게 들어가기
테스트를 작성할 때에는 다양한 함수들을 사용할 수 있습니다. 위에서 언급한 `assert`와 `expect` 외에도, `toBe` 함수 대신 `toEqual` 함수를 사용하여 깊은 비교를 할 수 있습니다. 또는 `toThrow` 함수를 사용하여 예외 상황을 검증할 수도 있습니다. 테스트를 작성할 때, 자주 사용될 수 있는 함수들을 익혀두는 것이 좋습니다.

## 또 다른 정보들
- [Jest 공식 문서](https://jestjs.io/docs/en/using-matchers)
- [Mocha 공식 문서](https://mochajs.org/#getting-started)
- [Jasmine 공식 문서](https://jasmine.github.io/tutorials/your_first_suite)

## 함께 보기
- [ES6 문법 정리](https://github.com/Cho-S/project-pig/blob/master/ES6%20%EB%AC%B8%EB%B2%95.md)
- [Javascript 비동기 처리](https://github.com/Cho-S/project-pig/blob/master/Asynchronous%20Javascript.md)