---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:15:25.642076-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"

category:             "Javascript"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하나요?)
자바스크립트에서 현재 날짜를 알아내는 것은 흔한 작업입니다. 사용자에게 날짜를 보여주거나, 특정 시간에 기반한 기능을 실행하기 위해 필요합니다.

## How to: (어떻게 하나요?)
```Javascript
// 현재 날짜와 시간을 가져오기
const now = new Date();
console.log(now);

// 현재 년도, 월, 일을 출력하기
console.log(now.getFullYear());    // 2023 (가정)
console.log(now.getMonth() + 1);   // 1 (1월이면, 0부터 시작하므로 +1을 해야함)
console.log(now.getDate());        // 15 (가정)
```
- 출력 예시:
```
2023-01-15T12:34:56.789Z
2023
1
15
```

## Deep Dive (심도 있는 정보)
자바스크립트의 `Date` 객체는 1995년 ECMAScript 언어 표준의 첫 번째 버전에서 소개되었습니다. 처음부터 현재 날짜와 시간을 제공하는 기본적인 방법이었죠. 대체할 수 있는 방법들, 예를 들면 `Date.now()`, `moment.js` 라이브러리 등이 존재하지만, `new Date()`는 가장 간단하고 자주 쓰이는 방법입니다. `get` 메서드들은 연도, 월, 일, 시간 등을 따로 가져올 수 있도록 해줍니다.

## See Also (관련 자료)
- Mozilla Developer Network (MDN) 'Date' 참조 문서: [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js 라이브러리 (다른 날짜 처리 방법을 제공함): [Moment.js](https://momentjs.com/)
- ECMAScript 현재 표준: [ECMAScript](https://www.ecma-international.org/ecma-262/)
