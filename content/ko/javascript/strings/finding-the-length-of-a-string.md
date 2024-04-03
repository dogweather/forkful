---
date: 2024-01-20 17:47:57.086573-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.778371-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## How to: (어떻게:)
```Javascript
let greeting = "안녕하세요!";
console.log(greeting.length);  // 출력: 6
```

자바스크립트에서 문자열의 길이를 알고 싶다면 `.length` 속성을 쓰세요. 간단하죠?

## Deep Dive (심층 분석)
문자열의 길이를 찾는 것은 자바스크립트가 탄생한 1995년부터 이미 존재하는 기능입니다. 이 `.length` 속성은 문자열 리터럴, 문자열 객체 모두에 적용됩니다. 유니코드 문자들도 1로 계산되니, 대부분 상황에서 잘 동작합니다.

UTF-16을 사용해 이모지나 특수 문자를 포함하는 문자열의 경우에는 조금 주의가 필요합니다. 이들은 때때로 "서로게이트 쌍"을 이루어 길이를 2로 계산할 수 있습니다. 따라서, 그런 특수한 경우에는 길이를 측정할 다른 방법이 필요할 수도 있습니다.

```Javascript
let specialChar = "😊";
console.log(specialChar.length);  // 출력: 2

let smiley = "😊".split(/(?=\uD83D)/u);
console.log(smiley.length);  // 출력: 1
```

위 예시처럼 `.split()` 함수와 정규 표현식을 활용하여 서로게이트 쌍을 정확히 측정할 수 있습니다.

## See Also (참고 자료)
- MDN Web Docs (문자열 길이): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length
- JavaScript 특수 문자 정규식 처리: https://www.regular-expressions.info/unicode.html
- 자바스크립트 유니코드: https://mathiasbynens.be/notes/javascript-unicode
