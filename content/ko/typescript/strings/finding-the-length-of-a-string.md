---
date: 2024-01-20 17:48:20.987307-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBB38\uC790\uC5F4\
  \ \uAE38\uC774\uB97C \uCC3E\uAE30 \uC704\uD55C `.length` \uC18D\uC131\uC740 JavaScript\uAC00\
  \ \uCC98\uC74C \uB4F1\uC7A5\uD55C 1995\uB144\uBD80\uD130 \uC874\uC7AC\uD574\uC654\
  \uB2E4. TypeScript\uB294 JavaScript\uB97C \uAE30\uBC18\uC73C\uB85C \uD558\uBBC0\uB85C\
  \ \uC774 \uC18D\uC131\uC744 \uADF8\uB300\uB85C \uACC4\uC2B9\uD55C\uB2E4. \uB300\uC548\
  \uC801\uC778 \uBC29\uBC95\uC740 \uAC70\uC758 \uC0AC\uC6A9\uB418\uC9C0 \uC54A\uC9C0\
  \uB9CC, \uBC30\uC5F4\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.266624-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBB38\uC790\uC5F4 \uAE38\uC774\
  \uB97C \uCC3E\uAE30 \uC704\uD55C `.length` \uC18D\uC131\uC740 JavaScript\uAC00 \uCC98\
  \uC74C \uB4F1\uC7A5\uD55C 1995\uB144\uBD80\uD130 \uC874\uC7AC\uD574\uC654\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to: (어떻게 하나요?)
```TypeScript
let greeting: string = "안녕하세요!";
let lengthOfGreeting: number = greeting.length;

console.log(lengthOfGreeting); // 출력: 6
```

## Deep Dive (심도 있는 탐구)
문자열 길이를 찾기 위한 `.length` 속성은 JavaScript가 처음 등장한 1995년부터 존재해왔다. TypeScript는 JavaScript를 기반으로 하므로 이 속성을 그대로 계승한다. 대안적인 방법은 거의 사용되지 않지만, 배열 변환 후 `Array.prototype.length`를 사용하는 방법도 있다. 이 길이 속성은 UTF-16 코드 유닛의 수를 반환하는데, 대부분의 경우 문자 수와 일치하지만 특수한 유니코드 문자(이모지나 특정 언어 문자들)는 더 많은 코드 유닛을 사용할 수 있다.

## See Also (더 보기)
- MDN Web Docs: [String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- TypeScript Handbook: [Basic Types](https://www.typescriptlang.org/docs/handbook/basic-types.html)
