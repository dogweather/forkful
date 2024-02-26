---
date: 2024-01-26 01:41:59.066380-07:00
description: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\
  \uB4DC\uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC678\uBD80\
  \ \uB3D9\uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC7AC\uAD6C\uC870\uD654\
  \uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294\
  \ \uC18C\uD504\uD2B8\uC6E8\uC5B4\uC758 \uBE44\uAE30\uB2A5\uC801 \uC18D\uC131\uC744\
  \ \uAC1C\uC120\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\
  \uBA70, \uCF54\uB4DC\uB97C \uB354 \uAE68\uB057\uD558\uACE0 \uD6A8\uC728\uC801\uC73C\
  \uB85C \uB9CC\uB4E4\uC5B4 \uC720\uC9C0 \uAD00\uB9AC\uB97C \uB2E8\uC21C\uD654\uD558\
  \uACE0 \uD5A5\uD6C4 \uAE30\uB2A5 \uCD94\uAC00\uB97C \uC6A9\uC774\uD558\uAC8C \uD569\
  \uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.791543-07:00'
model: gpt-4-0125-preview
summary: "\uB9AC\uD329\uD1A0\uB9C1\uC740 \uAE30\uC874 \uCEF4\uD4E8\uD130 \uCF54\uB4DC\
  \uC758 \uAD6C\uC870\uB97C \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC678\uBD80 \uB3D9\
  \uC791\uC744 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uACE0 \uC7AC\uAD6C\uC870\uD654\uD558\
  \uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC18C\
  \uD504\uD2B8\uC6E8\uC5B4\uC758 \uBE44\uAE30\uB2A5\uC801 \uC18D\uC131\uC744 \uAC1C\
  \uC120\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uBA70\
  , \uCF54\uB4DC\uB97C \uB354 \uAE68\uB057\uD558\uACE0 \uD6A8\uC728\uC801\uC73C\uB85C\
  \ \uB9CC\uB4E4\uC5B4 \uC720\uC9C0 \uAD00\uB9AC\uB97C \uB2E8\uC21C\uD654\uD558\uACE0\
  \ \uD5A5\uD6C4 \uAE30\uB2A5 \uCD94\uAC00\uB97C \uC6A9\uC774\uD558\uAC8C \uD569\uB2C8\
  \uB2E4."
title: "\uB9AC\uD329\uD130\uB9C1"
---

{{< edit_this_page >}}

## 무엇과 왜?
리팩토링은 기존 컴퓨터 코드의 구조를 변경하지 않고 외부 동작을 변경하지 않고 재구조화하는 과정입니다. 프로그래머는 소프트웨어의 비기능적 속성을 개선하기 위해 이 작업을 수행하며, 코드를 더 깨끗하고 효율적으로 만들어 유지 관리를 단순화하고 향후 기능 추가를 용이하게 합니다.

## 방법:

리팩토링이 코드를 더 간결하고 읽기 쉽게 만들 수 있는 간단한 예를 살펴봅시다. 여기에서 숫자 배열의 합계를 계산하는 함수를 리팩토링합니다.

리팩토링 전:
```javascript
function calculateSum(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }
  return sum;
}

console.log(calculateSum([1, 2, 3, 4])); // 출력: 10
```

리팩토링 후:
```javascript
function calculateSum(arr) {
  return arr.reduce((sum, num) => sum + num, 0);
}

console.log(calculateSum([1, 2, 3, 4])); // 출력: 10
```

`reduce` 메서드가 기능을 유지하면서 함수의 크기를 줄이는 방법을 봤나요? 바로 그것이 리팩토링입니다.

## 심층적으로

리팩토링은 마틴 파울러의 "Refactoring: Improving the Design of Existing Code"라는 책이 1999년에 출판될 때까지 공식적인 실천으로 자리잡지 않았습니다. 이 책은 민첩한 소프트웨어 개발의 부상과 함께 리팩토링을 주류로 밀어넣는데 도움을 주었습니다.

소프트웨어 개발의 한 측면으로 리팩토링을 설명하는 것은 작업장을 정돈하는 이유를 설명하는 것과 같습니다: 다음에 무언가를 고쳐야 할 때 (이 경우 코드), 어지러움을 다루는 데 시간을 덜 소비하고 실제 문제에 더 많은 시간을 소비하기 위해서입니다.

리팩토링에 대한 대안을 논할 때, 우리는 소프트웨어 유지 관리 전략에 대한 더 넓은 논의로 나아갑니다. 예를 들어 전체적인 재작성을 선택할 수 있지만, 그것은 종종 더 비싸고 위험합니다. 점진적으로 리팩토링하면 갑작스러운 전체 개편으로 인해 배를 가라앉히지 않고도 지속적인 혜택을 누릴 수 있습니다.

리팩토링은 통합 개발 환경(IDE)과 JSHint, ESLint, Prettier와 같은 JavaScript 생태계의 도구 개발에 도움을 받았으며, 이는 코드 품질 검사를 자동화하고 리팩토링 기회를 강조합니다.

이 모든 것은 깨끗하고 표현적이며 유지 관리가 가능한 코드에 관한 것입니다. 정교한 알고리즘, 데이터 구조 최적화 또는 절차적에서 함수형 프로그래밍 스타일로의 전환과 같은 아키텍처 변경이 리팩토링 과정의 일부일 수 있습니다.

리팩토링은 주의 깊게 수행되어야 합니다. 변화가 소프트웨어의 동작을 예상치 못하게 변경하지 않았음을 보장하기 위해 견고한 테스트 세트를 갖추고 있는 것이 필수적입니다—리팩토링과 잘 어우러지는 또 다른 이유는 기본적으로 안전망을 제공하기 때문입니다.

## 참고자료

- 마틴 파울러의 리팩토링 책: [Refactoring - Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- JavaScript 테스팅 프레임워크 (리팩토링이 기능을 해치지 않도록 보장하기 위해):
  - Jest: [Jest - Delightful JavaScript Testing](https://jestjs.io/)
  - Mocha: 
  [Mocha - the fun, simple, flexible JavaScript test framework](https://mochajs.org/)

- 코드 품질 및 리팩토링 지원 도구:
  - ESLint: [ESLint - Pluggable JavaScript linter](https://eslint.org/)
  - Prettier: [Prettier - Opinionated Code Formatter](https://prettier.io/)
