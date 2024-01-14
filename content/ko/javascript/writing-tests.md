---
title:                "Javascript: 테스트 작성"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

코드 테스트를 작성하는 이유는 단순입니다. 안정성과 신뢰성 있는 코드를 제공하기 위함입니다.

## 방법

코드 테스트를 작성하는 방법은 간단합니다. Javascript에서 코드를 작성하고 테스트를 실행하는 예시를 아래 코드 블록으로 확인하실 수 있습니다.

```Javascript
// 덧셈 함수 테스트
function sum(a, b) {
  return a + b;
}

// 예상 결과값: 5
console.log(sum(2,3));

// 예상 결과값: 11
console.log(sum(5,6));
```

위 코드에서는 덧셈 함수를 정의한 후, 예상 결과값을 기대하여 `console.log()`를 이용해 테스트하고 있습니다. 정상적으로 동작하는 경우 예상 결과값과 실제 결과값이 일치하여 테스트 성공으로 간주할 수 있습니다.

## 심층 분석

코드 테스트를 작성하면서 더 깊게 알아야 할 내용이 있습니다. 예를 들어, `assert` 함수를 사용하면 원하는 예상 값과 실제 값이 일치하는지 확인할 수 있습니다. 또한, `expect` 문법을 사용하면 코드가 예상한 결과를 반환하는지 여부를 검사할 수 있습니다. 이를 통해 더 간편하고 정확한 테스트를 작성할 수 있습니다.

## 참고 자료

- [JavaScript 테스트 프로그래밍 기초](https://d2.naver.com/helloworld/6423756)
- [테스트 주도 개발 (TDD)](https://edykim.com/ko/post/introduction-to-tdd/)
- [Jest를 이용한 자바스크립트 테스트 코드 작성하기](https://jeonghwan-kim.github.io/2018/05/28/javascript-test.html)

## 참고 자료

- [JavaScript Testing 101](https://d2.naver.com/helloworld/6423756)
- [Test-Driven Development (TDD)](https://edykim.com/en/post/introduction-to-tdd/)
- [Writing JavaScript test code using Jest](https://jeonghwan-kim.github.io/2018/05/28/javascript-test.html)