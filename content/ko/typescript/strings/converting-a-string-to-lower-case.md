---
date: 2024-01-20 17:39:32.064065-07:00
description: "How to: (\uBC29\uBC95) \uC0D8\uD50C \uCF54\uB4DC\uB294 'greeting'\uC774\
  \uB77C\uB294 \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\
  \uC5EC 'lowerCaseGreeting' \uBCC0\uC218\uC5D0 \uC800\uC7A5\uD569\uB2C8\uB2E4. \uACB0\
  \uACFC\uB97C \uCF58\uC194\uC5D0 \uCD9C\uB825\uD558\uBA74 'hello, world!'\uB97C \uBCFC\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.644590-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uC0D8\uD50C \uCF54\uB4DC\uB294 'greeting'\uC774\uB77C\uB294\
  \ \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uC5EC 'lowerCaseGreeting'\
  \ \uBCC0\uC218\uC5D0 \uC800\uC7A5\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to: (방법)
```TypeScript
let greeting: string = 'Hello, World!';
let lowerCaseGreeting: string = greeting.toLowerCase();

console.log(lowerCaseGreeting);  // 'hello, world!'
```
샘플 코드는 'greeting'이라는 문자열을 소문자로 변환하여 'lowerCaseGreeting' 변수에 저장합니다. 결과를 콘솔에 출력하면 'hello, world!'를 볼 수 있습니다.

## Deep Dive (심층 분석)
문자열을 소문자로 변환하는 기능은 모든 프로그래밍 언어에서 기본적으로 제공됩니다. JavaScript와 마찬가지로 TypeScript에서도 이를 `.toLowerCase()` 메서드를 통해 손쉽게 할 수 있습니다.

역사적 맥락에서 보면, 대소문자 변환이 필요한 이유 중 하나는 컴퓨터가 맨 처음 사용될 때 대문자만 인식할 수 있었기 때문입니다. 시간이 흐르고 기술이 발전하면서 소문자도 인식 가능해졌고, 이제는 사용자의 입력이나 데이터를 소문자로 바꾸는 것이 일반적인 처리가 되었습니다.

일부 언어나 환경에서는 기본 `.toLowerCase()` 외에도 정규표현식, 유틸리티 라이브러리 등을 사용하여 소문자로 변환할 수 있습니다. 이런 방법들은 추가적인 기능이나 사용자 정의 변환 규칙을 적용할 때 사용됩니다.

TypeScript에서 소문자 변환은 내부적으로 문자열의 각 문자를 ASCII 코드나 유니코드 매핑을 이용하여 대응하는 소문자로 바꾸는 과정을 거칩니다. 다른 언어들과 마찬가지로 로케일이나 언어 별 특수 케이스를 처리하는 복잡성이 내재해있습니다.

## See Also (참고 자료)
- MDN Web Docs의 `String.prototype.toLowerCase()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- TypeScript 공식 문서: https://www.typescriptlang.org/docs/
- Unicode 표준: http://www.unicode.org/standard/standard.html
