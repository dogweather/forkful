---
date: 2024-01-20 17:51:50.811969-07:00
description: "How to: (\uC2EC\uCE35 \uBD84\uC11D) \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740\
  \ ES6(ECMAScript 2015)\uC5D0\uC11C \uCC98\uC74C \uC18C\uAC1C\uB418\uC5C8\uACE0,\
  \ TypeScript\uC5D0\uC11C\uB3C4 \uC9C0\uC6D0\uD569\uB2C8\uB2E4. \uC774\uC804\uC5D0\
  \uB294 \uBB38\uC790\uC5F4\uC744 \uC5F0\uACB0\uD558\uAE30 \uC704\uD574 '+' \uC5F0\
  \uC0B0\uC790\uB97C \uC0AC\uC6A9\uD588\uC9C0\uB9CC, \uBCF4\uAC04\uC744 \uD1B5\uD574\
  \ \uC880 \uB354 \uAE54\uB054\uD558\uACE0 \uC9C1\uAD00\uC801\uC778 \uCF54\uB4DC\uB97C\
  \ \uC791\uC131\uD560 \uC218 \uC788\uAC8C \uB418\uC5C8\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.261611-06:00'
model: gpt-4-1106-preview
summary: "(\uC2EC\uCE35 \uBD84\uC11D) \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC740 ES6(ECMAScript\
  \ 2015)\uC5D0\uC11C \uCC98\uC74C \uC18C\uAC1C\uB418\uC5C8\uACE0, TypeScript\uC5D0\
  \uC11C\uB3C4 \uC9C0\uC6D0\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to:
(어떻게 사용하는가?)
```TypeScript
const name: string = '세계';
const greeting: string = `안녕하세요, ${name}!`;  // '안녕하세요, 세계!' 를 출력합니다.

console.log(greeting);
```

다중 라인에서도 사용 가능합니다:
```TypeScript
const itemCount: number = 5;
const pricePerItem: number = 2000;
const message: string = `총 비용은 ${itemCount * pricePerItem}원 입니다.`;

console.log(message);  // '총 비용은 10000원 입니다.' 를 출력합니다.
```

## Deep Dive
(심층 분석)
문자열 보간은 ES6(ECMAScript 2015)에서 처음 소개되었고, TypeScript에서도 지원합니다. 이전에는 문자열을 연결하기 위해 '+' 연산자를 사용했지만, 보간을 통해 좀 더 깔끔하고 직관적인 코드를 작성할 수 있게 되었습니다.

```TypeScript
// ES5 이전의 방식
var oldWay: string = '안녕하세요, ' + name + '!';
```

` ` (백틱) 과 `${expression}` (식을 포함하는 중괄호)을 사용합니다.
`${}` 안에는 모든 유형의 TypeScript 표현식을 삽입할 수 있습니다.

성능에 큰 차이는 없지만, 보간이 가독성과 유지보수 측면에서 이점을 제공합니다. 보간을 사용하면 템플릿 리터럴 내에서의 복잡한 표현식도 쉽게 처리할 수 있습니다.

## See Also
(추가 정보)
- TypeScript Handbook (Official): https://www.typescriptlang.org/docs/handbook/intro.html
- MDN Web Docs on Template Literals (영문): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- ES6 Features (영문): http://es6-features.org/#StringInterpolation
