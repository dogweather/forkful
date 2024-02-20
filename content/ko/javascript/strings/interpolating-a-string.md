---
date: 2024-01-20 17:51:07.611270-07:00
description: "String interpolation\uC740 \uBB38\uC790\uC5F4\uC5D0 \uBCC0\uC218\uB098\
  \ \uACC4\uC0B0 \uACB0\uACFC\uB97C \uC0BD\uC785\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4\
  . \uCF54\uB4DC\uB97C \uAC04\uACB0\uD558\uAC8C \uB9CC\uB4E4\uACE0 \uAC00\uB3C5\uC131\
  \uC744 \uB192\uC774\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.683169
model: gpt-4-1106-preview
summary: "String interpolation\uC740 \uBB38\uC790\uC5F4\uC5D0 \uBCC0\uC218\uB098 \uACC4\
  \uC0B0 \uACB0\uACFC\uB97C \uC0BD\uC785\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. \uCF54\
  \uB4DC\uB97C \uAC04\uACB0\uD558\uAC8C \uB9CC\uB4E4\uACE0 \uAC00\uB3C5\uC131\uC744\
  \ \uB192\uC774\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
String interpolation은 문자열에 변수나 계산 결과를 삽입하는 것입니다. 코드를 간결하게 만들고 가독성을 높이기 위해 사용합니다.

## How to: (어떻게:)
```javascript
// 변수를 사용한 보간법 (String Interpolation)
let name = "지수";
let greeting = `안녕, ${name}!`;
console.log(greeting); // 출력: 안녕, 지수!

// 표현식을 사용한 보간법
let price = 19000;
let taxRate = 0.1;
let total = `총 금액은 ${price + (price * taxRate)}원 입니다.`;
console.log(total); // 출력: 총 금액은 20900원 입니다.
```

## Deep Dive (심층 분석)
String interpolation은 ES6부터 시작된 자바스크립트의 기능입니다. 이전에는 문자열 연결을 위해 '+' 연산자를 사용했었죠. 예를 들어:

```javascript
// ES5 이전의 문자열 연결 방법
var oldGreeting = "안녕, " + name + "!";
console.log(oldGreeting); // 출력: 안녕, 지수!
```

이 방법은 길고 가독성이 떨어집니다. String interpolation은 템플릿 리터럴이라 불리는 ` ` 안에 `${}`를 사용하여 변수나 표현식을 쉽게 삽입할 수 있게 해줍니다. 이는 코드의 간결함을 증진시키고, 실수를 줄여줍니다.

또 하나의 대안은 문자열 포맷팅 함수를 사용하는 것인데, 이는 더 복잡하거나 특정 형식을 요구할 때 적합합니다.

## See Also (관련 자료)
- MDN 웹 문서의 템플릿 리터럴 (Template Literals): [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- ECMA-262, 6th Edition, The ECMAScript 2015 Language Specification: [https://www.ecma-international.org/ecma-262/6.0/](https://www.ecma-international.org/ecma-262/6.0/)
