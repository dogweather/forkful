---
date: 2024-01-20 17:47:57.086573-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC83\uC740\
  \ \uBB38\uC790\uC5F4\uC5D0 \uC788\uB294 \uBB38\uC790\uC758 \uC218\uB97C \uC138\uB294\
  \ \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\
  \uC774\uD130\uB97C \uAC80\uC99D\uD558\uAC70\uB098 \uD2B9\uC815 \uC5F0\uC0B0\uC744\
  \ \uD558\uAE30 \uC704\uD574 \uC774 \uC815\uBCF4\uB97C \uD544\uC694\uB85C \uD569\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.703234-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uB294 \uAC83\uC740 \uBB38\
  \uC790\uC5F4\uC5D0 \uC788\uB294 \uBB38\uC790\uC758 \uC218\uB97C \uC138\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\
  \uD130\uB97C \uAC80\uC99D\uD558\uAC70\uB098 \uD2B9\uC815 \uC5F0\uC0B0\uC744 \uD558\
  \uAE30 \uC704\uD574 \uC774 \uC815\uBCF4\uB97C \uD544\uC694\uB85C \uD569\uB2C8\uB2E4\
  ."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열의 길이를 찾는 것은 문자열에 있는 문자의 수를 세는 과정입니다. 프로그래머들은 데이터를 검증하거나 특정 연산을 하기 위해 이 정보를 필요로 합니다.

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
