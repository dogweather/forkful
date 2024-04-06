---
date: 2024-01-20 17:33:21.962496-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) - **\uC5ED\uC0AC\uC801\
  \ \uB9E5\uB77D**: \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC758 Date \uAC1D\uCCB4\uB294\
  \ 1995\uB144 \uC5B8\uC5B4\uAC00 \uCC98\uC74C \uB9CC\uB4E4\uC5B4\uC9C8 \uB54C\uBD80\
  \uD130 \uC874\uC7AC\uD588\uC2B5\uB2C8\uB2E4. - **\uB300\uC548**: \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC \uC5C6\uC774 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD560 \uC218 \uC788\uC9C0\
  \uB9CC, moment.js\uC640 \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\
  \uC6A9\uD558\uBA74 \uD3B8\uB9AC\uD569\uB2C8\uB2E4. - **\uAD6C\uD604\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:10.027673-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) - **\uC5ED\uC0AC\uC801 \uB9E5\uB77D\
  **."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## How to: (어떻게 하나요?)
```Javascript
// 두 날짜 객체 생성
const date1 = new Date('2023-04-01T00:00:00');
const date2 = new Date('2023-04-02T00:00:00');

// 날짜 비교
console.log(date1 < date2); // true, date1이 date2보다 이릅니다
console.log(date1 > date2); // false, date1이 date2보다 늦습니다
console.log(date1.getTime() === date2.getTime()); // false, 두 날짜는 같지 않습니다
```

## Deep Dive (심층 분석)
- **역사적 맥락**: 자바스크립트의 Date 객체는 1995년 언어가 처음 만들어질 때부터 존재했습니다. 
- **대안**: 라이브러리 없이 날짜를 비교할 수 있지만, moment.js와 같은 라이브러리를 사용하면 편리합니다.
- **구현 세부 사항**: 자바스크립트에서는 `getTime()` 메서드를 사용해 유닉스 타임스탬프로 변환하여 날짜를 비교합니다. 이 값은 1970년 1월 1일부터의 밀리초 수를 나타냅니다.

## See Also (더 보기)
- [Date 객체 - MDN Web Docs](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Date-fns library](https://date-fns.org/)
