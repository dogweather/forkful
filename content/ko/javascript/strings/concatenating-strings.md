---
title:                "문자열 연결하기"
aliases: - /ko/javascript/concatenating-strings.md
date:                  2024-01-20T17:35:20.711696-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 연결은 문자열들을 한데 붙여 새로운 문자열을 만드는 것입니다. 이는 데이터를 읽기 쉽게 결합하거나, 동적으로 텍스트를 생성할 때 유용합니다.

## How to: (어떻게 하나요?)
```javascript
// '+' 연산자 사용
let greeting = "안녕" + ", " + "세계!";
console.log(greeting); // "안녕, 세계!"

// 백틱(`)과 ${}를 사용한 템플릿 리터럴
let planet = "세계";
let helloWorld = `안녕, ${planet}!`;
console.log(helloWorld); // "안녕, 세계!"
```

## Deep Dive (심층 분석)
초창기 자바스크립트에서는 주로 '+' 연산자가 문자열을 합치는 데 사용되었습니다. 하지만 ES6(ES2015)부터는 템플릿 리터럴이라는 깔끔한 방법이 생겼죠. '`' 문자로 문자열을 감싸고, `${}` 구문으로 변수를 삽입합니다. 문자열 연결을 할 때, '+' 연산자는 메모리와 성능에 영향을 더 미칠 수 있는 반면, 템플릿 리터럴은 내부적으로 최적화가 잘 되어있어 성능상의 이점이 있습니다. 끝으로, 템플릿 리터럴은 멀티 라인 문자열을 만드는 것도 간단하게 해줍니다.

## See Also (관련 자료)
- MDN Web Docs의 [템플릿 리터럴](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Template_literals)
- MDN Web Docs의 [문자열](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)
