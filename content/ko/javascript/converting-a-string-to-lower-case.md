---
title:                "문자열을 소문자로 변환하기"
date:                  2024-01-20T17:38:52.142210-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 바꾼다는 것은, 모든 대문자를 그에 해당하는 소문자로 변경하는 것입니다. 대소문자를 구분하지 않는 문자열 비교나 검색을 할 때 주로 사용됩니다.

## How to: (방법)
```javascript
let greeting = 'Hello, World!';
let lowerCaseGreeting = greeting.toLowerCase();

console.log(lowerCaseGreeting); // "hello, world!"

let mixedCase = 'AnNyEoNg HaSeYo';
console.log(mixedCase.toLowerCase()); // "annyeong haseyo"
```

## Deep Dive (심층 분석)
문자열을 소문자로 변환하는 것은 웹 초기부터 필요했던 기능입니다. 데이터가 일관되게 저장되고 검색되어야 했기 때문이죠. `toLowerCase`는 JavaScript에서 이를 위한 표준적인 방법입니다.

대안으로 `toLocaleLowerCase` 메소드도 있습니다. 이 메소드는 특정 언어의 규칙에 따라 소문자로 변환합니다 (예: 터키어에서 I를 i로 변환할 때의 특수한 경우를 처리합니다).

JavaScript 내부에서 `toLowerCase`는 유니코드 캐릭터를 소문자 매핑으로 대체하면서 동작합니다. 하지만 성능이나 동작 방식은 브라우저나 자바스크립트 엔진에 따라 다를 수 있습니다.

## See Also (추가 자료)
- [`String.prototype.toLowerCase`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase) documentation on MDN Web Docs
- [`String.prototype.toLocaleLowerCase`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase) documentation on MDN Web Docs
- [Unicode character case mapping](https://unicode.org/faq/casemap_charprop.html) for understanding how characters are mapped to their lower case equivalents in different languages
