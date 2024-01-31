---
title:                "날짜를 문자열로 변환하기"
date:                  2024-01-20T17:36:47.298909-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇과 왜?)
자바스크립트에서 날짜를 문자열로 변환하는 건 날짜를 쉽게 읽고 저장하기 위한 작업입니다. 이 과정은 웹 페이지에 날짜를 표시하거나 날짜를 서버로 보낼 때 유용합니다.

## How to:
(방법:)
```javascript
// 현재 날짜와 시간 생성
const now = new Date();

// toString() 사용
console.log(now.toString()); // "Wed Mar 15 2023 21:00:00 GMT+0900 (한국 표준시)" 같이 출력됨

// toLocaleString() 사용 - 한국어 설정
console.log(now.toLocaleString('ko-KR')); // "2023. 3. 15. 오후 9:00:00" 같이 출력됨

// toISOString() 사용
console.log(now.toISOString()); // "2023-03-15T12:00:00.000Z" 같이 출력됨
```

## Deep Dive
(심층 분석)
날짜를 문자열로 변환하는 기능은 자바스크립트 초기 버전부터 있었습니다. toString(), toLocaleString(), toISOString() 같은 메서드는 날짜를 다양한 문자열 형태로 바꿔줍니다. toString()은 주로 디버깅할 때 사용하고, toLocaleString()은 사용자의 지역 설정에 맞는 형태로 날짜를 표현합니다. toISOString()은 날짜와 시간을 ISO 8601 형식으로 변환하여, 시간대와 관계없이 일정한 표현을 제공합니다. 각 메서드는 특정한 상황과 필요에 따라 적합하게 사용될 수 있습니다.

## See Also
(더 보기)
- MDN Web Docs Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Date.prototype.toString(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString
- Date.prototype.toLocaleString(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString
- Date.prototype.toISOString(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString
