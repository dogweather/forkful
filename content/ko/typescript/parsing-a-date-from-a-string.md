---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:39:30.263048-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜죠?)
문자열에서 날짜를 파싱한다는 것은 문자로 된 날짜 정보를 실제 날짜 타입으로 변환하는 과정입니다. 프로그래머들은 데이터 교환, 사용자 입력 처리, 타임스탬프 기록 등을 위해 이 작업을 합니다.

## How to: (어떻게 하나요?)
```typescript
const dateString: string = "2023-04-01"; // 날짜 형식 문자열
const parsedDate: Date = new Date(dateString); // 문자열 파싱하여 Date 객체 생성

console.log(parsedDate); // 출력: 2023-04-01T00:00:00.000Z (지역 시간대에 따라 다를 수 있음)
```

날짜와 시간 포맷을 직접 처리하고 싶다면:
```typescript
const dateString: string = "2023년 4월 1일 오후 3시";
const dateParts = dateString.match(/\d+/g); // 숫자만 추출

if (dateParts) {
    const year = parseInt(dateParts[0], 10);
    const month = parseInt(dateParts[1], 10) - 1; // JavaScript의 월은 0부터 시작
    const day = parseInt(dateParts[2], 10);
    const hour = parseInt(dateParts[3], 10) + (dateString.includes('오후') ? 12 : 0); // 오후 체크

    const customParsedDate = new Date(year, month, day, hour);
    console.log(customParsedDate); // 출력 형식: Sat Apr 01 2023 15:00:00 GMT+0900 (한국 표준시)
}
```

## Deep Dive (심층 분석)
과거에는 날짜와 시간을 다루는 일반적인 방법이 없었습니다. 각자 필요에 맞게 숫자와 문자열을 처리해야 했죠. 그러나 ECMAScript 5에서 `Date.parse`와 `new Date()`를 도입하며 이 문제가 일부 해결되었습니다. 자바스크립트의 `Date` 객체는 ISO 8601과 같은 표준 날짜 포맷을 지원합니다.

다만, 복잡한 날짜 형식이나 다양한 시간대를 처리할 때는 `Date` 객체의 한계에 부딪힙니다. 그래서 많은 개발자가 Moment.js 같은 날짜 관리 라이브러리를 선호합니다. 최근에는 Moment.js 대신 더 가볍고 현대적인 Date-fns나 Day.js 같은 라이브러리가 인기를 얻고 있습니다.

이런 라이브러리들은 보다 명확한 API로 날짜 파싱 기능을 제공하며, 복잡한 타임존 관리, 형식화된 문자열 출력, 상대적 날짜 계산 등의 기능도 갖추고 있습니다.

TypeScript에서 날짜 파싱은 자바스크립트와 동일하게 작동합니다. 다만, TypeScript는 타입 안전성을 제공하므로 변수나 함수가 반환하는 날짜 타입을 명확히 할 수 있습니다.

## See Also (더 보기)
- [MDN의 Date 객체 문서](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ISO 8601 날짜 표준](https://en.wikipedia.org/wiki/ISO_8601)
- [Date-fns 라이브러리](https://date-fns.org/)
- [Day.js 라이브러리](https://day.js.org/)