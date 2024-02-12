---
title:                "문자열의 길이 찾기"
aliases: - /ko/typescript/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:20.987307-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 길이 측정은 문자의 개수를 센다. 저장된 데이터 관리, 입력 유효성 검사, UI 디자인 조정에 필요하다.

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
