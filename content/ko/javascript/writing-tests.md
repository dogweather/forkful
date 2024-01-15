---
title:                "테스트 작성하기"
html_title:           "Javascript: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

테스트를 작성하는 이유는 프로그래밍의 디버깅 시간을 단축하고 코드의 신뢰성을 높이기 위해서입니다.

## 하는 법

```Javascript
// 전달 인자의 타입을 체크하는 단위 테스트 예시
function addNumbers(num1, num2) {
  if (typeof num1 !== 'number' || typeof num2 !== 'number') {
    throw TypeError('전달 인자는 숫자여야 합니다.');
  }
  return num1 + num2;
}
```

```Javascript
// 예상되는 결과값을 비교하는 단위 테스트 예시
function removeDuplicates(arr) {
  return [...new Set(arr)];
}
console.log(removeDuplicates([1, 2, 3, 3])); // 기대값: [1, 2, 3]
```

## 딥 다이브

테스트를 작성하면 코드를 작성함과 동시에 문제점을 일찍 발견하고 수정할 수 있습니다. 또한, 테스트는 코드의 변화에 따라 기존 코드가 제대로 작동하는지 확인할 수 있게 해주며, 코드를 리팩토링할 때 안정성을 보장할 수 있습니다.

## 다른 참고 자료

* [자바스크립트 단위 테스트를 위한 가이드](https://codeburst.io/a-guide-to-unit-testing-in-javascript-547e2dc19b7d)
* [자바스크립트 테스트를 위한 Jest 라이브러리](https://jestjs.io/)
* [테스트 주도 개발(TDD)에 대한 뉴스레터](https://www.bettersoftwareteam.com/newsletter/tdd/)