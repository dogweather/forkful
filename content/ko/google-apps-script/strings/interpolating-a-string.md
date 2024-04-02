---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:20.550535-07:00
description: "Google Apps Script\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740\
  \ \uBB38\uC790\uC5F4 \uB0B4\uC5D0\uC11C \uD45C\uD604\uC2DD\uC744 \uB3D9\uC801\uC73C\
  \uB85C \uC0BD\uC785\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uC5B4, \uB354 \uC77D\uAE30\
  \ \uC27D\uACE0 \uC720\uC9C0\uBCF4\uC218\uD558\uAE30 \uC26C\uC6B4 \uCF54\uB4DC \uC791\
  \uC131\uC744 \uAC00\uB2A5\uD558\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC774 \uAE30\uC220\uC744 \uC0AC\uC6A9\uD558\uC5EC \uBCC0\uC218\
  \uC640 \uD45C\uD604\uC2DD\uC744 \uBB38\uC790\uC5F4\uC5D0 \uBC88\uAC70\uB85C\uC6B4\
  \ \uC5F0\uACB0 \uAD6C\uBB38 \uC5C6\uC774 \uB9E4\uB044\uB7FD\uAC8C \uD1B5\uD569\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.505769-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 \uBB38\
  \uC790\uC5F4 \uB0B4\uC5D0\uC11C \uD45C\uD604\uC2DD\uC744 \uB3D9\uC801\uC73C\uB85C\
  \ \uC0BD\uC785\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uC5B4, \uB354 \uC77D\uAE30\
  \ \uC27D\uACE0 \uC720\uC9C0\uBCF4\uC218\uD558\uAE30 \uC26C\uC6B4 \uCF54\uB4DC \uC791\
  \uC131\uC744 \uAC00\uB2A5\uD558\uAC8C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uC774 \uAE30\uC220\uC744 \uC0AC\uC6A9\uD558\uC5EC \uBCC0\uC218\
  \uC640 \uD45C\uD604\uC2DD\uC744 \uBB38\uC790\uC5F4\uC5D0 \uBC88\uAC70\uB85C\uC6B4\
  \ \uC5F0\uACB0 \uAD6C\uBB38 \uC5C6\uC774 \uB9E4\uB044\uB7FD\uAC8C \uD1B5\uD569\uD569\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## 무엇 & 왜?

Google Apps Script에서 문자열 보간은 문자열 내에서 표현식을 동적으로 삽입할 수 있게 해주어, 더 읽기 쉽고 유지보수하기 쉬운 코드 작성을 가능하게 합니다. 프로그래머들은 이 기술을 사용하여 변수와 표현식을 문자열에 번거로운 연결 구문 없이 매끄럽게 통합합니다.

## 방법:

Google Apps Script에서 문자열 보간은 템플릿 리터럴을 통해 달성됩니다. 이는 백틱(`)을 사용하여 표현식을 포함할 수 있는 문자열 리터럴입니다. 사용 방법은 다음과 같습니다:

```javascript
// 기본 예제
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`안녕하세요, ${user}님!`); // 출력: 안녕하세요, Alice님!
}

// 표현식 사용하기
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`다섯 더하기 열은 ${a + b}입니다.`); // 출력: 다섯 더하기 열은 15입니다.
}

// 다중 라인 문자열
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`다중 라인 문자열입니다:
여러분 안녕하세요,
오늘은 ${item}에 대해 논의하겠습니다.`);
  // 출력:
  // 다중 라인 문자열입니다:
  // 여러분 안녕하세요,
  // 오늘은 Google Apps Script에 대해 논의하겠습니다.
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

이 예시들은 기본 사용법, 표현식 삽입, 보간된 값을 사용한 다중 라인 문자열 생성을 보여줍니다.

## 깊이 있게 알아보기

템플릿 리터럴, 문자열 보간을 포함하여 ECMAScript 2015 (ES6)에서 도입되었고 이후 Google Apps Script에서 채택되었습니다. 이전에는 프로그래머들이 복잡한 문자열이나 많은 변수 값을 통합할 때 너무 번거로울 수 있는 순수하게 문자열 연결에 의존해야 했습니다.

```javascript
// 이전 방법 (ES6 이전)
var user = 'Bob';
console.log('Hello, ' + user + '!');
```

문자열 보간은 강력한 기능이지만, 사용하는 컨텍스트에 유의해야 합니다. 예를 들어, 적절한 정화 없이 사용자 입력을 직접 포함시키는 것은 주입 공격과 같은 보안 문제로 이어질 수 있습니다. Google Apps Script 개발자들은 문자열에 보간된 동적 콘텐츠가 적절하게 확인되거나 정화되었는지 확인해야 합니다.

다른 프로그래밍 언어와 비교할 때, 문자열 보간 개념은 널리 존재하며 문법이 다양합니다. 파이썬은 f-strings 또는 `format` 메소드를 사용하고, 루비는 쌍따옴표 문자열 내에서 `#{}`을 사용하며, 많은 현대 언어들이 제공하는 가독성과 편리함 때문에 비슷한 기능을 채택하고 있습니다.

Google Apps Script는 ECMAScript 표준에서 제공하는 것 이상의 추가 보간 기능을 제공하지 않지만, 현재의 기능은 대부분의 사용 사례에 충분히 강력하고 유용합니다. 더 복잡한 보간 메커니즘을 사용하는 언어에서 온 개발자들은 자신의 기대를 조정할 필요가 있을 수 있지만, Google Apps Script에서 템플릿 리터럴의 단순성과 효율성을 확실히 높이 평가할 것입니다.
